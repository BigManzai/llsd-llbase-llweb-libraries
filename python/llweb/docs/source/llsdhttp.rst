########
llsdhttp
########

.. module:: llsdhttp
   :synopsis: Functions to ease moving llsd over http.

This module is a wrapper for llsd through the httpc client suite. This
module imports httpc.ConnectionError, httpc.Retriable, and all derived
exceptions so will not need to import httpc to catch client exceptions.

All of the public functions of this module are wrappers around an
:class:`llweb.httpc.HttpSuite` instance with Accept and Content-Type
headers of ``application/llsd+xml.``


.. autofunction::llweb.llsdhttp.get

.. function:: get(url[, headers[, use_proxy[, ok[, max_retries[, connection]]]])

  Issue an HTTP GET to the specified url.

  :param url: The url to get.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param use_proxy: set TRUE if you want to perform a get through an
     http proxy. Default False.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param max_retries: Maximum nuber of times to try the get. Default 8.
  :param connection: The connection (as returned by
     :func:`llweb.httpc.make_connection`) to use for the request.
  :returns: Returns the parsed response from the server as a python object.

.. function:: get_(url[, headers[, use_proxy[, ok[, max_retries[, connection]]]])

  Issue an HTTP GET to the specified url.

  :param url: The url to get.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param use_proxy: set TRUE if you want to perform a get through an
     http proxy. Default False.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param max_retries: Maximum nuber of times to try the get. Default 8.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns tuple (status, msg, paylod) where status is the
     status code, msg is :class:`httplib.HTTPMessage`, and payload is
     the parsed response from the server as a python object.


.. function:: head(url[, headers[, use_proxy[, ok[, connection]]]])

  Issue an HTTP HEAD to the specified url.

  :param url: The url to head.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param use_proxy: set TRUE if you want to perform a head through an
     http proxy. Default False.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns the parsed response from the server as a python object.

.. function:: head_(url[, headers[, use_proxy[, ok[, connection]]]])

  Issue an HTTP HEAD to the specified url.

  :param url: The url to head.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param use_proxy: set TRUE if you want to perform a head through an
     http proxy. Default False.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns tuple (status, msg, paylod) where status is the
     status code, msg is :class:`httplib.HTTPMessage`, and payload is
     the parsed response from the server as a python object.


.. function:: options(url[, headers[, use_proxy[, ok[, connection]]]])

  Issue an HTTP OPTIONS to the specified url.

  :param url: The url to options.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param use_proxy: set TRUE if you want to perform a options through an
     http proxy. Default False.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns the parsed response from the server as a python object.

.. function:: options_(url[, headers[, use_proxy[, ok[, connection]]]])

  Issue an HTTP OPTIONS to the specified url.

  :param url: The url to options.
  :param data: A python object to serialize as llsd and POST to url.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param content_type: Intended MIME-type of the data. Default None
     which will assume the MIME-type of the suite.
  :param use_proxy: set TRUE if you want to perform a options through an
     http proxy. Default False.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns tuple (status, msg, paylod) where status is the
     status code, msg is :class:`httplib.HTTPMessage`, and payload is
     the parsed response from the server as a python object.


.. function:: post(url, data[, headers[, content_type[, ok[, connection]]]])

  Issue an HTTP POST to the specified url.

  :param url: The url to post.
  :param data: A python object to serialize as llsd and POST to url.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param content_type: Intended MIME-type of the data. Default None
     which will assume the MIME-type of the suite.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns the parsed response from the server as a python object.

.. function:: post_(url, data[, headers[, content_type[, ok[, connection]]]])

  Issue an HTTP POST to the specified url.

  :param url: The url to post.
  :param data: A python object to serialize as llsd and POST to url.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param content_type: Intended MIME-type of the data. Default None
     which will assume the MIME-type of the suite.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns tuple (status, msg, paylod) where status is the
     status code, msg is :class:`httplib.HTTPMessage`, and payload is
     the parsed response from the server as a python object.


.. function:: put(url, data[, headers[, content_type[, ok[, connection]]]])

  Issue an HTTP PUT to the specified url.

  :param url: The url to put.
  :param data: A python object to serialize as llsd and PUT to url.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param content_type: Intended MIME-type of the data. Default None
     which will assume the MIME-type of the suite.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns the parsed response from the server as a python object.

.. function:: put_(url, data[, headers[, content_type[, ok[, connection]]]])

  Issue an HTTP PUT to the specified url.

  :param url: The url to put.
  :param headers: Dict of headers to pass in. No need for the accept header.
  :param content_type: Intended MIME-type of the data. Default None
     which will assume the MIME-type of the suite.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns tuple (status, msg, paylod) where status is the
     status code, msg is :class:`httplib.HTTPMessage`, and payload is
     the parsed response from the server as a python object.


.. function:: delete(url[, ok[, connection]])

  Issue an HTTP DELETE to the specified url.

  :param url: The url to delete.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns the parsed response from the server as a python object.


.. function:: delete_(url[, ok[, connection]])

  Issue an HTTP DELETE to the specified url.

  :param url: The url to delete.
  :param ok: Set of valid response statuses. If the returned status is
     not in this list, an exception is thrown.
  :param connection: The connection (as returned by
     httpc.make_connection) to use for the request.
  :returns: Returns tuple (status, msg, paylod) where status is the
     status code, msg is :class:`httplib.HTTPMessage`, and payload is
     the parsed response from the server as a python object.
