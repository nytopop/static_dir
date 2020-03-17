//! A crate for embedding static assets into warp webservers.
#![warn(rust_2018_idioms, missing_docs)]

use futures::future::{self, Ready};
use headers::{
    AcceptRanges, ContentLength, ContentRange, ContentType, HeaderMapExt, IfModifiedSince, IfRange,
    IfUnmodifiedSince, LastModified, Range,
};
use http::StatusCode;
use hyper::Body;
use once_cell::sync::Lazy;
use proc_macro_hack::proc_macro_hack;
use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};
use warp::{
    filters::header::headers_cloned,
    path::{self, Tail},
    reject::{self, Rejection},
    reply::Response,
    Filter,
};

#[doc(hidden)]
#[proc_macro_hack]
pub use include_dir::include_dir;

#[doc(hidden)]
pub use include_dir::{Dir, File};

/// Creates a [Filter][warp::Filter] that serves a directory at the base `$path` joined by
/// the request path.
///
/// It behaves much like warp's [fs::dir][warp::filters::fs::dir], but instead of serving
/// files from the filesystem at runtime, the directory to be served will be embedded into
/// your binary at compile time.
///
/// If the provided path is relative, it will be relative to the project root (as per the
/// `CARGO_MANIFEST_DIR` environment variable).
///
/// # Examples
/// ```ignore
/// use static_dir::static_dir;
/// use warp::Filter;
///
/// // Matches requests that start with `/static`, and then uses the
/// // rest of that path to lookup and serve a file from `/www/static`.
/// let route = warp::path("static").and(static_dir!("/www/static"));
///
/// // For example:
/// // - `GET /static/app.js` would serve the file `/www/static/app.js`
/// // - `GET /static/css/app.css` would serve the file `/www/static/css/app.css`
/// ```
#[macro_export]
macro_rules! static_dir {
    ($path:literal) => {{
        static DIR: ::static_dir::Dir = ::static_dir::include_dir!($path);

        ::static_dir::filter(&DIR)
    }};
}

#[doc(hidden)]
/// Creates a [Filter][warp::Filter] that serves an included directory.
///
/// See the documentation of [static_dir] for details.
pub fn filter(
    dir: &'static Dir<'_>,
) -> impl Filter<Extract = (Response,), Error = Rejection> + Clone {
    warp::get()
        .and(path_from_tail(dir))
        .and(headers_cloned().map(conds))
        .and_then(move |path, conds| reply(dir, path, conds))
}

fn path_from_tail(
    d: &'static Dir<'_>,
) -> impl Filter<Extract = (PathBuf,), Error = Rejection> + Clone {
    path::tail().and_then(move |tail| future::ready(parse_path(tail, &d)))
}

fn parse_path(tail: Tail, dir: &Dir<'_>) -> Result<PathBuf, Rejection> {
    let mut path: PathBuf = urlencoding::decode(tail.as_str())
        .map_err(|_| reject::not_found())
        .map(|path| path.split('/').collect())?;

    if path.parent().is_none() || dir.get_dir(&*path).is_some() {
        log::debug!("static_dir: appending index.html to {:?}", path);
        path.push("index.html");
    }

    Ok(path)
}

#[derive(Clone, Debug, Default)]
struct Conditionals {
    if_modified_since: Option<IfModifiedSince>,
    if_unmodified_since: Option<IfUnmodifiedSince>,
    if_range: Option<IfRange>,
    range: Option<Range>,
}

enum Cond {
    WithBody(Option<Range>),
    NoBody(Response),
}

impl Conditionals {
    fn check(self, sys_time: &SystemTime, last_mod: &LastModified) -> Cond {
        if let Some(since) = self.if_unmodified_since {
            if !since.precondition_passes(*sys_time) {
                let mut res = Response::new(Body::empty());
                *res.status_mut() = StatusCode::PRECONDITION_FAILED;
                return Cond::NoBody(res);
            }
        }

        if let Some(since) = self.if_modified_since {
            if !since.is_modified(*sys_time) {
                let mut res = Response::new(Body::empty());
                *res.status_mut() = StatusCode::NOT_MODIFIED;
                return Cond::NoBody(res);
            }
        }

        if let Some(if_range) = self.if_range {
            if if_range.is_modified(None, Some(last_mod)) {
                return Cond::WithBody(None);
            }
        }

        Cond::WithBody(self.range)
    }

    fn serve(self, buf: &'static [u8], path: &Path) -> Response {
        static SYS_TIME: Lazy<SystemTime> = Lazy::new(|| SystemTime::now());
        static LAST_MOD: Lazy<LastModified> = Lazy::new(|| (*SYS_TIME).into());

        match self.check(&SYS_TIME, &LAST_MOD) {
            Cond::WithBody(Some(range)) => match bytes_range(range, buf.len() as u64) {
                Some((0, e)) if e == buf.len() as u64 => reply_full(buf, path, *LAST_MOD),
                Some(range) => reply_range(buf, range, path, *LAST_MOD),
                None => reply_unsatisfiable(buf.len() as u64),
            },
            Cond::WithBody(None) => reply_full(buf, path, *LAST_MOD),
            Cond::NoBody(resp) => resp,
        }
    }
}

fn bytes_range(range: Range, max_len: u64) -> Option<(u64, u64)> {
    use std::ops::Bound;

    let (start, end) = range.iter().next()?;

    let start = match start {
        Bound::Unbounded => 0,
        Bound::Included(s) => s,
        Bound::Excluded(s) => s + 1,
    };

    let end = match end {
        Bound::Unbounded => max_len,
        Bound::Included(s) => s + 1,
        Bound::Excluded(s) => s,
    };

    if start < end && end <= max_len {
        Some((start, end))
    } else {
        log::trace!(
            "static_dir: unsatisfiable byte range: {}-{}/{}",
            start,
            end,
            max_len
        );
        Some((0, max_len))
    }
}

fn conds<H: HeaderMapExt>(h: H) -> Conditionals {
    Conditionals {
        if_modified_since: h.typed_get(),
        if_unmodified_since: h.typed_get(),
        if_range: h.typed_get(),
        range: h.typed_get(),
    }
}

type Filtered<T> = Ready<Result<T, Rejection>>;

fn reply(dir: &'static Dir<'_>, path: PathBuf, conds: Conditionals) -> Filtered<Response> {
    let r = if let Some(file) = dir.get_file(&*path) {
        log::debug!("static_dir: serving: {:?}", path);
        Ok(conds.serve(file.contents(), path.as_ref()))
    } else {
        log::warn!("static_dir: file not found: {:?}", path);
        Err(reject::not_found())
    };

    future::ready(r)
}

fn reply_range(
    buf: &'static [u8],
    (s, e): (u64, u64),
    path: &Path,
    last_mod: LastModified,
) -> Response
{
    let len = e - s;
    let buf = &buf[s as usize..e as usize];
    assert_eq!(len, buf.len() as u64);

    let mut resp = Response::new(Body::from(buf));
    *resp.status_mut() = StatusCode::PARTIAL_CONTENT;

    let hdrs = resp.headers_mut();
    hdrs.typed_insert(ContentRange::bytes(s..e, len).unwrap());
    hdrs.typed_insert(ContentLength(len));

    let mime = mime_guess::from_path(path).first_or_octet_stream();
    hdrs.typed_insert(ContentType::from(mime));
    hdrs.typed_insert(AcceptRanges::bytes());
    hdrs.typed_insert(last_mod);

    resp
}

fn reply_full(buf: &'static [u8], path: &Path, last_mod: LastModified) -> Response {
    let mut resp = Response::new(buf.into());
    let hdrs = resp.headers_mut();

    hdrs.typed_insert(ContentLength(buf.len() as u64));
    let mime = mime_guess::from_path(path).first_or_octet_stream();
    hdrs.typed_insert(ContentType::from(mime));
    hdrs.typed_insert(AcceptRanges::bytes());
    hdrs.typed_insert(last_mod);

    resp
}

fn reply_unsatisfiable(len: u64) -> Response {
    let mut resp = Response::new(Body::empty());

    *resp.status_mut() = StatusCode::RANGE_NOT_SATISFIABLE;
    resp.headers_mut()
        .typed_insert(ContentRange::unsatisfied_bytes(len));

    resp
}
