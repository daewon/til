class Html
  def content(s) 
    print(s)
  end 

  def method_missing *args, &block
    tag, option = args[0], args[1] ||= {}
    option_str = " " + option.map { |k, v| "#{k}='#{v}'" }.join(" ")

    print "<#{tag}#{option_str.strip}>"
    instance_eval &block if block_given?
    print "</#{tag}>"
  end
end

builder = Html.new
builder.HTML(lang: :ko) do
  HEAD do
    SCRIPT(src: "/app.js", language: "javascript")
    SCRIPT(src: "/logger.js", language: "javascript")
  end

  BODY(style: "display:none;") do
    content("daewon")
    P(display: 'block') do
      [1, 2, 3].each do |n|
        LI(index: n) { content(n) }
      end
    end
  end
end
