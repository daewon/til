# coding: utf-8

require 'json'
require 'uri'
require 'net/http'
require 'nokogiri'

module Http
  def self.get(uri, headers = {}, options = {})
    req = Net::HTTP::Get.new(uri)
    headers.each do |k, v|
      req[k] = v
    end

    Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(req)
    end
  end

  def self.post(uri, payload, headers = {}, retry_cnt = 3, options = {})
    http = Net::HTTP.new(uri.host, uri.port)
    # http.set_debug_output $stderr

    http.read_timeout = 10
    http.open_timeout = 10
    http.keep_alive_timeout = 30

    begin
      http.start do |client|
        request = Net::HTTP::Post.new(uri.path)
        request.body = payload
        headers.each { |k, v| request[k] = v }
        ret = client.request(request)

        # client.finish
        ret
      end
    rescue Exception => e
      STDERR.puts e
      raise e if retry_cnt == 0
      send(uri, payload, headers, retry_cnt = retry_cnt - 1)
    end
  end
end

class KbdManaia
  def initialize(passwd)
    @passwd = passwd
  end

  def getAuthToken()
    payload = "error_return_url=%2Fxe%2Findex.php%3Fact%3DdispMemberLoginForm&mid=new_home&vid=&ruleset=%40login&success_return_url=http%3A%2F%2Fwww.kbdmania.net%2Fxe%2Fmarket&act=procMemberLogin&user_id=blueiur&password=#{@passwd}"
    headers = {
      'content-type' => 'application/x-www-form-urlencoded',
      'X-Requested-With' => 'XMLHttpRequest',
      'accept-encoding' => 'none',
      'xe_logged' => 'true',
      'Host' => 'www.kbdmania.net',
      'Referer' => 'http://www.kbdmania.net/xe/index.php?mid=new_home',
      'user-agent' => 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.50 Safari/537.36'
    }

    login_url = URI.parse("http://www.kbdmania.net/xe/")
    ret = Http.post(login_url, payload, headers)

    ret = ret.to_hash['set-cookie']
    puts ret

    if ret.empty?
      puts "login failed: #{ret.inspect}"
      exit(1)
    else 
      puts "login success: #{ret.inspect}"
    end

    ret.join
  end

  def getPage(cookie)
    market_uri = URI.parse('http://www.kbdmania.net/xe/market')
    headers = {
      'Accept' => 'text/html',
      'Cookie' => cookie,
      'Accept-Encoding' => 'none',
      'Host' => 'www.kbdmania.net',
      'Referer' => 'http://www.kbdmania.net/xe/index.php?mid=>new_home',
      'xe_logged' => 'true',
      'User-Agent' => 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.50 Safari/537.36',
    }

    Http.get(market_uri, headers).body
  end
end

kbd = KbdManaia.new ARGV.first
cookie = kbd.getAuthToken

seen = {}
loop do
  ret = kbd.getPage(cookie)
  text = ret.force_encoding('utf-8')
  html_doc = Nokogiri::HTML(text)

  target = html_doc.css('.boardList .title')
  target = target.reject { |title| title.css('.category').text == '거래완료' }
  target = target.map { |title| title.css('a').text }

  selected = target.select do |title|
    seen[title.downcase.hash] ? false : /해피|hhkp|포커|poker|hacker|abko/ =~ title.downcase
  end

  selected.each do |txt|
    seen[txt.downcase.hash] = 10
  end

  puts selected
  puts "_" * 80

  sleep(1)
end
