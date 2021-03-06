#!/usr/bin/env python
"""\
@file rub_test.py
@brief Test the config module

$LicenseInfo:firstyear=2009&license=mit$

Copyright (c) 2009, Linden Research, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
$/LicenseInfo$
"""

import cgi
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import time
import sys
from threading import Thread, Event
from unittest import TestCase, main

try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

from llweb import httpc

def _get_query_pairs(env):
    parsed = cgi.parse_qs(env['QUERY_STRING'])
    for key, values in parsed.items():
        for val in values:
            yield key, val

def get_query_pairs(env):
    return list(_get_query_pairs(env))

class BasicHandler(BaseHTTPRequestHandler):
    stuff = {'hello':'hello world'}

    def log_request(*args, **kwargs):
        pass
    def log_error(*args, **kwargs):
        pass

    def do_GET(self):
        env = {'REQUEST_METHOD':'GET'}
        self.do_it(env)

    def do_HEAD(self):
        env = {'REQUEST_METHOD':'HEAD'}
        self.do_it(env)

    def do_PUT(self):
        env = {'REQUEST_METHOD':'PUT'}
        self.do_it(env)

    def do_DELETE(self):
        env = {'REQUEST_METHOD':'DELETE'}
        self.do_it(env)

    def do_POST(self):
        env = {'REQUEST_METHOD':'POST'}
        self.do_it(env)

    def do_it(self, env):
        env.update(self.collect_env())
        response = self(env, self.start_response)
        if response:
            for resp in response:
                self.wfile.write(resp)
        else:
                self.wfile.write('')
            

    def call(self, env, start_response):
        return getattr(self, 'handle_%s' % env['REQUEST_METHOD'].lower())(env, start_response)

    def __call__(self, env, start_response):
        return self.call(env, start_response)

    def start_response(self, status, headers):
        message = status[4:]
        status_code = int(status[:3])
        self.send_response(status_code)#, message)
        for name, value in headers:
            self.send_header(name, value)
        self.end_headers()

    def collect_env(self):
        env = {}
        path = self.path.split('?')
        env['PATH_INFO'] = path[0]
        env['wsgi.input'] = self.rfile
        env['CONTENT_LENGTH'] =int(self.headers.getheader('content-length', 0))
        env['HTTP_HOST'] = 'localhost:31337'
        if '?' in self.path:
            env['QUERY_STRING'] = path[1]
        else:
            env['QUERY_STRING'] = ''
        return env

class EventletHandler(BasicHandler):
    def handle_get(self, env, start_response):
        headers = [('x-get', 'hello'), ('Content-type', 'text/plain')]
        resp = StringIO()
        path = env['PATH_INFO'].lstrip('/')
        try:
            resp.write(self.stuff[path])
        except KeyError:
            start_response("404 Not Found", headers)
            return ["Not Found"]
        for k,v in get_query_pairs(env):
            resp.write(k + '=' + v + '\n')
        start_response("200 OK", headers)
        return [resp.getvalue()]

    def handle_head(self, env, start_response):
        headers = [('x-head', 'hello'), ('Content-type', 'text/plain')]
        start_response("200 OK", headers)
        path = env['PATH_INFO'].lstrip('/')
        return [""]

    def handle_put(self, env, start_response):
        headers = [('x-put', 'hello'), ('Content-type', 'text/plain')]
        path = env['PATH_INFO'].lstrip('/')
        if not path:
            start_response("400 Bad Request", headers)
            return [""]

        if path in self.stuff:
            start_response("204 No Content", headers)
        else:
            start_response("201 Created", headers)
        self.stuff[path] = env['wsgi.input'].read(int(env.get('CONTENT_LENGTH', '0')))
        return [""]

    def handle_delete(self, env, start_response):
        headers = [('x-delete', 'hello'), ('Content-type', 'text/plain')]
        path = env['PATH_INFO'].lstrip('/')
        if not path:
            start_response("400 Bad Request", headers)
            return [""]
        try:
            del self.stuff[path]
            start_response("204 No Content", headers)
        except KeyError:
            start_response("404 Not Found", headers)
        return [""]

    def handle_post(self, env, start_response):
        headers = [('x-post', 'hello'), ('Content-type', 'text/plain')]
        start_response("200 OK", headers)
        return [env['wsgi.input'].read(int(env.get('CONTENT_LENGTH', '0')))]


class TestServer(Thread):
    def __init__(self, working, listening, handler):
        super(TestServer, self).__init__()
        self.working = working
        self.listening = listening
        self.handler = handler

    def run(self):
        httpd = None
        import socket
        while httpd is None:
            try:
                httpd = HTTPServer(('', 31337), self.handler)
            except socket.error, ee:
                if ee[0] != 98:
                    raise ee
        self.listening.set()
        while self.working.isSet():
            httpd.handle_request()
        self.listening.clear()

class TestBase(object):
    site_class = EventletHandler

    def _root(self):
        return 'http://localhost:31337/'

    def base_url(self):
        return self._root()

    def setUp(self):
        self.working = Event()
        self.working.set()
        self.listening = Event()
        self.server = TestServer(self.working, self.listening, self.site_class)
        self.server.start()
        while not self.listening.isSet():
            time.sleep(0.05)

    def tearDown(self):
        self.working.clear()
        try:
            httpc.head(self._root() + 'diediedie')
        except Exception:
            pass
        while self.listening.isSet():
            time.sleep(0.05)

class TestHttpc(TestBase, TestCase):
    def test_get_bad_uri(self):
        self.assertRaises(httpc.NotFound,
                          lambda: httpc.get(self.base_url() + 'b0gu5'))

    def test_get(self):
        response = httpc.get(self.base_url() + 'hello')
        self.assertEquals(response, 'hello world')

    def test_get_(self):
        status, msg, body = httpc.get_(self.base_url() + 'hello')
        self.assertEquals(status, 200)
        self.assertEquals(msg.dict['x-get'], 'hello')
        self.assertEquals(body, 'hello world')

    def test_get_query(self):
        response = httpc.get(self.base_url() + 'hello?foo=bar&foo=quux')
        self.assertEquals(response, 'hello worldfoo=bar\nfoo=quux\n')

    def test_head_(self):
        status, msg, body = httpc.head_(self.base_url() + 'hello')
        self.assertEquals(status, 200)
        self.assertEquals(msg.dict['x-head'], 'hello')
        self.assertEquals(body, '')

    def test_head(self):
        self.assertEquals(httpc.head(self.base_url() + 'hello'), '')

    def test_post_(self):
        data = 'qunge'
        status, msg, body = httpc.post_(self.base_url() + '', data=data)
        self.assertEquals(status, 200)
        self.assertEquals(msg.dict['x-post'], 'hello')
        self.assertEquals(body, data)

    def test_post(self):
        data = 'qunge'
        self.assertEquals(httpc.post(self.base_url() + '', data=data),
                          data)

    def test_put_bad_uri(self):
        self.assertRaises(
            httpc.BadRequest,
            lambda: httpc.put(self.base_url() + '', data=''))

    def test_put_empty(self):
        httpc.put(self.base_url() + 'empty', data='')
        self.assertEquals(httpc.get(self.base_url() + 'empty'), '')

    def test_put_nonempty(self):
        data = 'nonempty'
        httpc.put(self.base_url() + 'nonempty', data=data)
        self.assertEquals(httpc.get(self.base_url() + 'nonempty'), data)

    def test_put_01_create(self):
        try:
            # for some reason, unittest calls this function twice (!)
            response = httpc.get(self.base_url() + 'goodbye')
            return
        except httpc.NotFound:
            pass
        data = 'goodbye world'
        status, msg, body = httpc.put_(self.base_url() + 'goodbye',
                                       data=data)
        self.assertEquals(status, 201)
        self.assertEquals(msg.dict['x-put'], 'hello')
        self.assertEquals(body, '')
        self.assertEquals(httpc.get(self.base_url() + 'goodbye'), data)

    def test_put_02_modify(self):
        self.test_put_01_create()
        data = 'i really mean goodbye'
        status = httpc.put_(self.base_url() + 'goodbye', data=data)[0]
        self.assertEquals(status, 204)
        self.assertEquals(httpc.get(self.base_url() + 'goodbye'), data)

    def test_delete_(self):
        httpc.put(self.base_url() + 'killme', data='killme')
        status, msg, body = httpc.delete_(self.base_url() + 'killme')
        self.assertEquals(status, 204)
        self.assertRaises(
            httpc.NotFound,
            lambda: httpc.get(self.base_url() + 'killme'))

    def test_delete(self):
        httpc.put(self.base_url() + 'killme', data='killme')
        self.assertEquals(httpc.delete(self.base_url() + 'killme'), '')
        self.assertRaises(
            httpc.NotFound,
            lambda: httpc.get(self.base_url() + 'killme'))

    def test_delete_bad_uri(self):
        self.assertRaises(
            httpc.NotFound,
            lambda: httpc.delete(self.base_url() + 'b0gu5'))



class RedirectSite(EventletHandler):
    response_code = "301 Moved Permanently"

    def __call__(self, env, start_response):
        path = env['PATH_INFO']
        if path.startswith('/redirect/'):
            url = 'http://' + env['HTTP_HOST'] + path.replace('/redirect/', '/')
            start_response(self.response_code, [("Location", url)])
            return [""]
        return self.call(env, start_response)

class Site301(RedirectSite):
    pass

class Site302(EventletHandler):
    def __call__(self, env, start_response):
        path = env['PATH_INFO']
        if path.startswith('/expired/'):
            url = 'http://' + env['HTTP_HOST'] + path.replace('/expired/', '/')
            headers = [('location', url), ('expires', '0')]
            start_response("302 Found", headers)
            return [""]
        if path.startswith('/expires/'):
            url = 'http://' + env['HTTP_HOST'] + path.replace('/expires/', '/')
            expires = time.time() + (100 * 24 * 60 * 60)
            headers = [('location', url), ('expires', httpc.to_http_time(expires))]
            start_response("302 Found", headers)
            return [""]
        return self.call(env, start_response)

class Site303(RedirectSite):
    response_code = "303 See Other"


class Site307(RedirectSite):
    response_code = "307 Temporary Redirect"


class TestHttpc301(TestBase, TestCase):
    site_class = Site301

    def base_url(self):
        return 'http://localhost:31337/redirect/'

    def test_get(self):
        try:
            httpc.get(self.base_url() + 'hello', max_retries=0)
            self.assert_(False)
        except httpc.MovedPermanently, err:
            response = err.retry()
        self.assertEquals(response, 'hello world')
        self.assertEquals(httpc.get(self.base_url() + 'hello', max_retries=1), 'hello world')

# This fails some times for reasons that is hard to figure out.
#     def test_post(self):
#         data = 'qunge'
#         try:
#             response = httpc.post(self.base_url() + '', data=data)
#             self.assert_(False)
#         except httpc.MovedPermanently, err:
#             response = err.retry()
#         self.assertEquals(response, data)


class TestHttpc302(TestBase, TestCase):
    site_class = Site302

    def test_get_expired(self):
        try:
            httpc.get(self.base_url() + 'expired/hello', max_retries=0)
            self.assert_(False)
        except httpc.Found, err:
            response = err.retry()
        self.assertEquals(response, 'hello world')
        self.assertEquals(httpc.get(self.base_url() + 'expired/hello', max_retries=1), 'hello world')

    def test_get_expires(self):
        try:
            httpc.get(self.base_url() + 'expires/hello', max_retries=0)
            self.assert_(False)
        except httpc.Found, err:
            response = err.retry()
        self.assertEquals(response, 'hello world')
        self.assertEquals(httpc.get(self.base_url() + 'expires/hello', max_retries=1), 'hello world')


# class TestHttpc303(TestBase, TestCase):
#     site_class = Site303

#     def base_url(self):
#         return 'http://localhost:31337/redirect/'

#     def test_post(self):
#         data = 'hello world'
#         try:
#             response = httpc.post(self.base_url() + 'hello', data=data)
#             self.assert_(False)
#         except httpc.SeeOther, err:
#             response = err.retry()
#         self.assertEquals(response, data)


# class TestHttpc307(TestBase, TestCase):
#     site_class = Site307

#     def base_url(self):
#         return 'http://localhost:31337/redirect/'

#     def test_post(self):
#         data = 'hello world'
#         try:
#             response = httpc.post(self.base_url() + 'hello', data=data)
#             self.assert_(False)
#         except httpc.TemporaryRedirect, err:
#             response = err.retry()
#         self.assertEquals(response, data)


class Site500(EventletHandler):
    def __call__(self, env, start_response):
        start_response("500 Internal Server Error", [("Content-type", "text/plain")])
        return ["screw you world"]


class TestHttpc500(TestBase, TestCase):
    site_class = Site500

    def base_url(self):
        return 'http://localhost:31337/'

    def test_get(self):
        data = 'screw you world'
        try:
            response = httpc.get(self.base_url())
            self.fail()
        except httpc.InternalServerError, e:
            self.assertEquals(e.params.response_body, data)
            self.assert_(str(e).count(data))
            self.assert_(repr(e).count(data))


class Site504(EventletHandler):
    def __call__(self, env, start_response):
        start_response("504 Gateway Timeout", [("Content-type", "text/plain")])
        return ["screw you world"]


class TestHttpc504(TestBase, TestCase):
    site_class = Site504

    def base_url(self):
        return 'http://localhost:31337/'

    def test_post(self):
        # Simply ensure that a 504 status code results in a
        # GatewayTimeout.  Don't bother retrying.
        self.assertRaises(httpc.GatewayTimeout,
                          lambda: httpc.get(self.base_url()))


class TestHttpTime(TestCase):
    rfc1123_time = 'Sun, 06 Nov 1994 08:49:37 GMT'
    rfc850_time  = 'Sunday, 06-Nov-94 08:49:37 GMT'
    asctime_time = 'Sun Nov  6 08:49:37 1994'
    secs_since_epoch = 784111777
    def test_to_http_time(self):
        self.assertEqual(self.rfc1123_time, httpc.to_http_time(self.secs_since_epoch))

    def test_from_http_time(self):
        for formatted in (self.rfc1123_time, self.rfc850_time, self.asctime_time):
            ticks = httpc.from_http_time(formatted, 0)
            self.assertEqual(ticks, self.secs_since_epoch)

if __name__ == '__main__':
    main()
