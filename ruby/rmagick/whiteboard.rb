require 'rubygems'
require 'RMagick'
include Magick

fname = "IMG_0764.jpg"
puts "loading image"
img = ImageList.new(fname)

# scale to 800x600, full size image processing a pay $$ serivce?
scale_factor = 800.0 / img.page.width
new_height   = scale_factor * img.page.height
puts "downscaling by #{scale_factor} to get from (#{img.page.height}x#{img.page.height}) to (#{new_height}x800)"
img.scale! scale_factor

# 1. dup the layer
# 2. gaussian blur
#      http://studio.imagemagick.org/RMagick/doc/image2.html#gaussian_blur
puts "gaussian_blur"
blurred_layer = img[0].gaussian_blur 25, 3.0

# 3. invert colors
top_layer = ImageList.new
puts "inverting colors"
top_layer << blurred_layer.negate

# 4. dodge and merge
puts "compositing w/Dodging"
dodged = img.composite_layers top_layer, ColorDodgeCompositeOp

puts "adjusting levels"
dodged = dodged.level(0.5,0.5)

# 5. emit the result
puts "writing result"
dodged.write "cleaned-#{fname}"

# TODO: look into other transformations: 
#     increase the contrast
#     adjust brightness
#     adjust levels -- more stretching
#     look into white threshold -- maybe take the average pixel and make it white?
