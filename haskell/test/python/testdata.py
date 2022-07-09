# file testdata.py
#
# $LicenseInfo:firstyear=2009&license=mit$
#
# Copyright (c) 2009, Linden Research, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# $/LicenseInfo$


class LogItem (object):
    @staticmethod
    def fromLLSD(l):
        # FIXME: should handle malformed LLSD here
        event = l["event"]
        if event == "login":
            item = LogIn()
        elif event == "logout":
            item = LogOut()
        elif event == "misc":
            item = LogMisc()
        else:
            return LogError()
        item.readFromLLSD(l)
        return item
        
        
    
class LogIn (LogItem):
    def toLLSD(self):
        return {
            "event":        "login",
            "agent":        self.agent,
            "time":         self.time,
            "firsttime":    self.first_time,
            "referer":      self.referer
        }
        
    def readFromLLSD(self, l):
        self.agent          = l["agent"]
        self.time           = l["time"]
        self.first_time     = l["firsttime"]
        self.referer        = l["referer"]
        
    def tally(self, stats):
        stats.count_in += 1



class LogOut (LogItem):
    def toLLSD(self):
        return {
            "event":        "logout",
            "agent":        self.agent,
            "time":         self.time,
            "attachments":  self.attachments.toLLSD()
        }
        
    def readFromLLSD(self, l):
        self.agent          = l["agent"]
        self.time           = l["time"]
        self.attachments    = Attachments.fromLLSD(l["attachments"])
        
    def tally(self, stats):
        stats.count_out += 1
        stats.note_attachments(self.attachments)
        
        
        
class LogMisc (LogItem):
    def toLLSD(self):
        return {
            "event":        "misc",
            "agent":        self.agent,
            "time":         self.time,
            "this":         self.this,
            "that":         self.that
        }
        
    def readFromLLSD(self, l):
        self.agent          = l["agent"]
        self.time           = l["time"]
        self.this           = l["this"]
        self.that           = l["that"]
        
    def tally(self, stats):
        stats.count_misc += 1



class LogError (LogItem):
    def toLLSD(self):
        return None
        
    def tally(self, stats):
        stats.count_error += 1



class Attachments (object):
    def toLLSD(self):
        l = [ ]
        for (k, v) in self.attachments:
            l += { "point": k, "inv": v }
        return l

    @staticmethod
    def fromLLSD(l):
        a = Attachments()
        a.readFromLLSD(l)
        return a
        
    def readFromLLSD(self, l):
        a = [ ]
        for m in l:
            k = m["point"]
            v = m["inv"]
            a += [(k, v)]
        self.attachments = a
    
    def length(self):
        return len(self.attachments)



class LogStats (object):
    def __init__(self):
        self.count_in = 0
        self.count_out = 0
        self.count_misc = 0
        self.count_error = 0
        self.attachments_histogram = [ ]
    
    def __str__(self):
        return "LogStats { in = %d, out = %d, misc = %d, error = %d,\n   attachments_histogram = %s }" % (
            self.count_in, self.count_out,
            self.count_misc, self.count_error,
            str(self.attachments_histogram))
            
    def note(self, item):
        item.tally(self)

    def note_attachments(self, attachments):
        l = attachments.length()
        nl = l + 1
        hl = len(self.attachments_histogram)
        if nl > hl:
            self.attachments_histogram += ([0] * (nl - hl))
        self.attachments_histogram[l] += 1
