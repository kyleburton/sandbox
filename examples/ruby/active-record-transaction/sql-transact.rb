CONN = ActiveRecord::Base.connection

# expects to have a table with 2 fields (id,str_field)
table_name = "test_table"
field = 'str_field'

puts "Hit enter to start the process"
input = $stdin.readline

CONN.transaction do
  #CONN.execute("SET TRANSACTION ")
  id = '175576003'
  recs = CONN.select_all("SELECT * FROM #{table_name} WHERE id=#{id} FOR UPDATE")
  value = recs.first[field].to_i
  puts "before: #{recs.first[field]}"

  puts "Hit enter to update the #{field}"
  input = $stdin.readline
  value += 1
  res = CONN.execute("UPDATE #{table_name} set #{field}='#{value}' where id=#{id}")
  p res.inspect

  puts "Hit enter to update the carrier"
  input = $stdin.readline
  res = CONN.execute("UPDATE #{table_name} set carrier='new-carrier-#{$$}' where id=#{id}")
  p res.inspect

  recs = CONN.select_all("SELECT * FROM #{table_name} WHERE id=#{id}")
  puts "after:  #{recs.first[field]}"

end
