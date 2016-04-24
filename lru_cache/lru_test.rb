#!/usr/bin/env ruby
gem 'minitest', '>= 5.0.0'
require 'minitest/autorun'
require_relative 'lru'

class LruTest < Minitest::Test
  def test_lru
    max_size = 3

    cache = Lru.new(max_size)
    cache.put('a', 1)
    puts cache.head.value.inspect
    cache.put('b', 2)
    puts cache.last.value.inspect
    cache.put('c', 3)
    puts cache.last.value.inspect
    cache.put('d', 4)
    puts cache.last.value.inspect
    cache.put('e', 5)
    puts cache.last.value.inspect
    cache.put('f', 6)

    puts cache.get('e').inspect

    puts cache.head.value.inspect

    assert cache.get('a').nil?
    # assert_equal cache.size, max_size
  end
end
