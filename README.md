# Resourcerer

## Introduction

Resourcerer is a library for writing RESTful APIs. Unlike many other such
libraries, Resourcerer's model is based on *resources*, which may support a
number of operations that match the RESTful model's semantics; Resourcerer then
takes care of routing requests to the right resources, performing content type
negotiation, resolving HTTP methods, and tagging things with appropriate
HATEOAS metadata.

In other words, you describe how a particular resource produces its data, and
how to get and manipulate its child resources, hand it to Resourcerer, and you
get a working RESTful API.

## Concepts

A **Resource** is the core building block of an API; you pass a root resource
to Resourcerer, which defines a tree that maps to a tree of URLs. A resource
may implement any subset of the RESTful operations: get self, list children,
get child, insert child, replace child, delete child; leaving out any of these
operations leads to the correct HTTP response, namely "405 Method Not Allowed".

Note that unlike typical RESTful API libraries, Resourcerer does not
distinguish between *collections* (things that contain other things) and
*documents* (things that do not contain other things, but have a body); both
are modelled as just *resources*, and a resource can act as either, or both,
depending on which methods you do and do not implement.

In order to support both rich HATEOAS-tagged self-describing responses as well
as exact binary ones, resources can produce bodies in two distinct formats:

- A **Structured Body** is a response body that supports exactly the same data
  structures as JSON does (and in fact, resources return JSON `Value`s).
  Resourcerer automatically tags these with HATEOAS metadata, and converts them
  to any of the supported formats based on the request's `Accept` header.
  Resourcerer will also automatically add digests of child resources to the
  response.
- A **Binary Body** is a binary response body, which is served exactly as the
  resource returns it. Given a list of MIME types and corresponding body
  producers, Resourcerer will pick the most appropriate one and use it.

## The API Request Lifecycle

1. **HTTP request parsing**. Resourcerer receives an HTTP request from Wai, and
   parses out the relevant information - `Accept` header, path, query string,
   request method, etc.
2. **Method dispatch**. In order to figure out what to do, Resourcerer first
   looks at the request method. Each method has its own handler, but they all
   start at the root resource.
3. **Resource dispatch**. Resourcerer now consumes path elements from the
   request path, one by one, and traverses the resource tree while doing so.
4. **Calling resource methods**. Depending on the requested method, the
   appropriate resource methods are now called, and some content type
   negotiation happens. For destructive methods (PUT, POST, DELETE), this step
   also reads and parses the request body and performs content negotiation for
   it.
5. **Resource composition**. The results from the resource method calls are
   assembled into an API response object.
5. **Failure handling**. If any of the above steps failed, a failure response
   is created instead.
6. **Final content negotiation**. The API response is compared to the `Accept`
   header, and either rejected or transformed into something acceptable.
7. **Response rendering and sending**. The API response is translated to a Wai
   Response object, and sent to the client.
