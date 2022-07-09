Linden Lab LLSD Libraries
-------------------------
    v1 - 2010-06-07 - Joshua Bell & Mark Lentczner
    v2 - 2010-06-08 - Joshua Bell & Mark Lentczner


ABOUT
=====

LLSD is defined in draft-hamrick-vwrap-type-system-00. It is a structured data
system used for interchange. See the draft for more details:

    http://tools.ietf.org/html/draft-hamrick-vwrap-type-system-00

The design and development of LLSD is being undertaken by the IETF VWRAP
working group. Please direct design and development questions to its mailing
list.

    wiki - http://trac.tools.ietf.org/wg/vwrap/trac/wiki
    list - https://www.ietf.org/mailman/listinfo/vwrap
    
This distribution contains Linden Lab's open source (MIT licensed)
implementations of the LLSD type system in five langauges:

    C++         LLSD
    JavaScript  LLSD & LLIDL
    Haskell     LLSD & LLIDL
    PHP         LLSD (partial)
    Python      LLSD & LLIDL (*)
    Ruby        LLSD (partial)

Each language has its own directory, and each has its own README file with
details for building and using the code. All versions have unit tests.

Questions regarding these libraries can be directed to Josh:
    josh@lindenlab.com

Linden Lab hopes that by releasing these open source, it will help facilitiate
the development of LLSD, as well as ease interoperatibility with currently
deployed systems that use LLSD such as Second Life and OpenSim.

    - Josh & Mark
    
(*) The Python version is currently distributed in a separate repository:
    http://hg.secondlife.com/llbase/
The intention is to eventually migrate it here.

