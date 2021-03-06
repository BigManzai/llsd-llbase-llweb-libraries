# file llsd_test.rb
#
# $LicenseInfo:firstyear=2007&license=mit$
#
# Copyright (c) 2007-2010, Linden Research, Inc.
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

require(File.join(File.dirname(File.dirname(__FILE__)), 'llsd'))

# if ran as a script, run unit tests
if __FILE__ == $0
  require 'test/unit'

  class LLSDUnitTest < Test::Unit::TestCase
    def setup
      # LLSD = LLSD.new
    end

    #~ def teardown
    #~ end

    def test_map
      map_xml = <<EOF
      <llsd>
      <map>
       <key>foo</key>
       <string>bar</string>
      </map>
      </llsd>
EOF

      map_within_map_xml = <<EOF
      <llsd>
      <map>
       <key>doo</key>
       <map>
         <key>goo</key>
         <string>poo</string>
       </map>
      </map>
      </llsd>
EOF

      blank_map_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <map />
      </llsd>
EOF

      ruby_map = {"foo" => "bar"}
      ruby_map_within_map = {"doo" => {"goo" => "poo"}}

      assert_equal ruby_map, LLSD.parse(map_xml)
      assert_equal ruby_map_within_map, LLSD.parse(map_within_map_xml)
      assert_equal({}, LLSD.parse(blank_map_xml))

      assert_equal strip(map_xml), LLSD.to_xml(ruby_map)
      assert_equal strip(map_within_map_xml), LLSD.to_xml(ruby_map_within_map)
    end

    def test_array
      array_xml = <<EOF
      <llsd>
      <array>
        <string>foo</string>
        <string>bar</string>
      </array>
      </llsd>
EOF

      array_within_array_xml = <<EOF
      <llsd>
      <array>
        <string>foo</string>
        <string>bar</string>
        <array>
          <string>foo</string>
          <string>bar</string>
        </array>
      </array>
      </llsd>
EOF

      blank_array_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <array />
      </llsd>
EOF

      ruby_array = ["foo", "bar"]
      ruby_array_within_array = ["foo", "bar",["foo", "bar"]]

      assert_equal ruby_array,  LLSD.parse(array_xml)
      assert_equal ruby_array_within_array,  LLSD.parse(array_within_array_xml)
      assert_equal([], LLSD.parse(blank_array_xml))

      assert_equal strip(array_xml), LLSD.to_xml(ruby_array)
      assert_equal strip(array_within_array_xml), LLSD.to_xml(ruby_array_within_array)
    end

    def test_string
      normal_xml = <<EOF
      <llsd>
      <string>foo</string>
      </llsd>
EOF

      blank_xml = <<EOF
      <llsd>
      <string />
      </llsd>
EOF

      assert_equal "foo", LLSD.parse(normal_xml)
      assert_equal "", LLSD.parse(blank_xml)

      assert_equal strip(normal_xml), LLSD.to_xml("foo")
      assert_equal strip(blank_xml), LLSD.to_xml("")
    end

    def test_integer
      pos_int_xml = <<EOF
      <llsd>
      <integer>289343</integer>
      </llsd>
EOF

      neg_int_xml = <<EOF
      <llsd>
      <integer>-289343</integer>
      </llsd>
EOF

      blank_int_xml = <<EOF
      <llsd>
      <integer />
      </llsd>
EOF

      ruby_pos_int = 289343
      ruby_neg_int = -289343

      assert_equal ruby_pos_int, LLSD.parse(pos_int_xml)
      assert_equal ruby_neg_int, LLSD.parse(neg_int_xml)
      assert_equal 0, LLSD.parse(blank_int_xml)

      assert_equal strip(pos_int_xml), LLSD.to_xml(ruby_pos_int)
      assert_equal strip(neg_int_xml), LLSD.to_xml(ruby_neg_int)
    end

    def test_real
      pos_real_xml = <<EOF
      <llsd>
      <real>2983287453.38483</real>
      </llsd>
EOF

      neg_real_xml = <<EOF
      <llsd>
      <real>-2983287453.38483</real>
      </llsd>
EOF

      blank_real_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <real />
      </llsd>
EOF
      ruby_pos_real = 2983287453.38483
      ruby_neg_real = -2983287453.38483

      assert_equal ruby_pos_real, LLSD.parse(pos_real_xml)
      assert_equal ruby_neg_real, LLSD.parse(neg_real_xml)
      assert_equal 0, LLSD.parse(blank_real_xml)

      assert_equal strip(pos_real_xml), LLSD.to_xml(ruby_pos_real)
      assert_equal strip(neg_real_xml), LLSD.to_xml(ruby_neg_real)
    end

    def test_boolean
      true_xml = <<EOF
      <llsd>
      <boolean>true</boolean>
      </llsd>
EOF

      false_xml = <<EOF
      <llsd>
      <boolean>false</boolean>
      </llsd>
EOF

      blank_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <boolean />
      </llsd>
EOF

      assert_equal true, LLSD.parse(true_xml)
      assert_equal false, LLSD.parse(false_xml)
      assert_equal false, LLSD.parse(blank_xml)

      assert_equal strip(true_xml), LLSD.to_xml(true)
      assert_equal strip(false_xml), LLSD.to_xml(false)
    end

    def test_date
      valid_date_xml = <<EOF
      <llsd>
      <date>2006-02-01T14:29:53Z</date>
      </llsd>
EOF

      blank_date_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <date />
      </llsd>
EOF

      ruby_valid_date = DateTime.strptime("2006-02-01T14:29:53Z")
      ruby_blank_date = DateTime.strptime("1970-01-01T00:00:00Z")

      assert_equal(ruby_valid_date, LLSD.parse(valid_date_xml))
      assert_equal(ruby_blank_date, LLSD.parse(blank_date_xml))

      assert_equal strip(valid_date_xml), LLSD.to_xml(ruby_valid_date)
    end

    # because the following types dont have "native" types in ruby, they convert to string

    def test_binary
      base64_binary_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <binary>dGhlIHF1aWNrIGJyb3duIGZveA==</binary>
      </llsd>
EOF

      # <binary /> should return blank binary blob... in ruby I guess this is just nil

      assert_equal "dGhlIHF1aWNrIGJyb3duIGZveA==", LLSD.parse(base64_binary_xml)
    end

    def test_uuid
      valid_uuid_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <uuid>d7f4aeca-88f1-42a1-b385-b9db18abb255</uuid>
      </llsd>
EOF

      blank_uuid_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <uuid />
      </llsd>
EOF

      assert_equal 'd7f4aeca-88f1-42a1-b385-b9db18abb255', LLSD.parse(valid_uuid_xml)
      assert_equal '00000000-0000-0000-0000-000000000000', LLSD.parse(blank_uuid_xml)
    end

    def test_uri
      valid_uri_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <uri>http://www.example.com:4201/agents</uri>
      </llsd>
EOF

      blank_uri_xml = <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <llsd>
      <uri />
      </llsd>
EOF

      # <uri /> should return an empty link, which in ruby I guess is just nil
      assert_equal 'http://www.example.com:4201/agents', LLSD.parse(valid_uri_xml)
      assert_equal nil, LLSD.parse(blank_uri_xml)
    end

    def test_undefined
      undef_xml = <<EOF
      <llsd><undef /></llsd>
EOF

      assert_equal nil, LLSD.parse(undef_xml)
      assert_equal strip(undef_xml), LLSD.to_xml(nil)
    end

    def test_llsd_serialization_exception
      # make an object not supported by llsd
      ruby_range = Range.new 1,2

      # assert than an exception is raised
      assert_raise(LLSDSerializationError){ LLSD.to_xml(ruby_range) }
    end


    def strip(str)
      str.delete "\n "
    end
    #~ def test_fail
      #~ assert(false, 'Assertion was false.')
    #~ end
  end
end
