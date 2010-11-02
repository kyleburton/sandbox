
npas = (100..799).to_a
nxxs = (100..899).to_a
nums = (0..10000).to_a
max = 1_000_000 * 1

max.times do 
  i1 = rand(npas.count)
  i2 = rand(nxxs.count)
  i3 = rand(nums.count)
  puts sprintf("(%03d) %03d-%04d", npas[i1], nxxs[i2], nums[i3])
end

# npas.each do |npa|
#   nxxs.each do |nxx|
#     nums.times do |num|
#       puts sprintf("(%03d) %03d-%04d", npa, nxx, num)
#     end
#   end
# end
