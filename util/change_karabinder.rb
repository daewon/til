require 'json'

file_name = '/Users/daewon/.karabiner.d/configuration/karabiner.json'

config = JSON.parse File.open(file_name).read

profile_names = config['profiles'].map { |profile| profile['name'] }

list = profile_names.each_with_index.map { |name, idx| "#{idx+1}: #{name}" }

puts "Select KBD Profile:"

puts ""
puts list.join("\n")
puts ""
# puts profile_names

select = gets.to_i - 1

config['profiles'].each do |profile|
  if profile['name'] == profile_names[select]
    profile['selected'] = true
    puts "Current Profile is [#{profile['name']}]"
  else
    profile['selected'] = false
  end
end

File.open(file_name, 'w') do |file|
  new_value = JSON.pretty_generate(config)
  file.write(new_value)
end
