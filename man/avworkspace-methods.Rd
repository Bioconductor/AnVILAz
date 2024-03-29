% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avworkspace-methods.R
\name{avworkspace-methods}
\alias{avworkspace-methods}
\alias{avworkspaces,azure-method}
\alias{avworkspace_namespace,azure-method}
\alias{avworkspace_name,azure-method}
\alias{avworkspace,azure-method}
\alias{avworkspace_clone,azure-method}
\title{AnVIL Azure Workspace methods}
\usage{
\S4method{avworkspaces}{azure}(..., platform = cloud_platform())

\S4method{avworkspace_namespace}{azure}(..., platform = cloud_platform())

\S4method{avworkspace_name}{azure}(..., platform = cloud_platform())

\S4method{avworkspace}{azure}(..., platform = cloud_platform())

\S4method{avworkspace_clone}{azure}(
  namespace = avworkspace_namespace(),
  name = avworkspace_name(),
  to_namespace = namespace,
  to_name,
  bucket_location = "US",
  ...,
  platform = cloud_platform()
)
}
\arguments{
\item{...}{Additional arguments passed to lower level functions (not used)}

\item{platform}{\code{azure()} The cloud platform class to dispatch on as given by
\link[AnVILBase:cloud_platform]{AnVILBase::cloud_platform}. Typically not set manually as
\code{cloud_platform()} returns the \code{"azure"} class for Azure workspaces on
AnVIL.}

\item{namespace}{\code{character(1)} AnVIL workspace namespace as returned by,
e.g., \code{avworkspace_namespace()}}

\item{name}{\code{character(1)} AnVIL workspace name as returned by, eg.,
\code{avworkspace_name()}.}

\item{to_namespace}{\code{character(1)} workspace (billing account) in
which to make the clone.}

\item{to_name}{\code{character(1)} name of the cloned workspace.}

\item{bucket_location}{\code{character(1)} region in which bucket
attached to the workspace should be created. The default is set to a
single region ("US"); multi-region is available but more costly.}
}
\description{
AnVIL Azure Workspace methods
}
\section{Functions}{
\itemize{
\item \code{avworkspaces(azure)}: List workspaces

\item \code{avworkspace_namespace(azure)}: List the workspace namespace

\item \code{avworkspace_name(azure)}: Obtain the workspace name

\item \code{avworkspace(azure)}: Obtain the current workspace namespace and
name combination

\item \code{avworkspace_clone(azure)}: Clone a workspace

}}
