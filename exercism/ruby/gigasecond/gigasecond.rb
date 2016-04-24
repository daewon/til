#!/usr/bin/env ruby
module Gigasecond
  GIGA_SECOND = 1_000_000_000

  def self.from(date)
    Time.at(date.to_i + GIGA_SECOND)
  end
end
