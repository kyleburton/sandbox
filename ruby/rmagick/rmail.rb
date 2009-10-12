#!/usr/bin/env ruby
require 'rubygems'
require 'tmail'
require 'RMagick'
require 'net/smtp'
include Magick
require 'actionmailer'

class Emailer < ActionMailer::Base
  def image_email(to_addr, image_data)
    recipients to_addr
    subject "Hopefully a Cleaner Whiteboard Photo"
    from "kyle.burton@gamail.com"
    body "You should have an image attached.  Thank you for trying Scribble Mail and Please come again."
    attachment :content_type => "image/jpeg", :body => image_data
  end
end

def process_image(send_to, jpeg_data)
  puts "jpeg data.size=#{jpeg_data.size}"
  img = ImageList.new
  img.from_blob(jpeg_data)
  
  # TODO: choose the wider dimension and diminish to the hieght/width bounds

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
  #puts "writing result"
  #dodged.write "cleaned-#{fname}"

  # TODO: look into other transformations: 
  #     increase the contrast
  #     adjust brightness
  #     adjust levels -- more stretching
  #     look into white threshold -- maybe take the average pixel and make it white?
  
  # base4 encode it and send it back out!

  ## can't seem to send a valid img with TMail
  #mail = TMail::Mail.new
  #mail.to = send_to
  #mail.from = "kyle.burton@gmail.com"
  #mail.subject = "Hopefully a Cleaner Whiteboard Photo"
  #mail.date = Time.now
  #mail.mime_version = '1.0'
  #mail.set_content_type 'image', 'jpeg'

  #puts "calling to_blob, format=#{dodged.format}"
  #dodged.write "cleaned.jpg"

  #mail.body = dodged.to_blob
  #puts "sending back to: #{send_to}"

  #Net::SMTP.start('localhost',25) do |smtp|
  #  smtp.send_message mail.to_s, 'kyle.burton@gmail.com', send_to
  #end

  ## try with actionmailer...
  Emailer.deliver_image_email send_to, dodged.to_blob
  puts "sent!"
end

#mail = TMail::Mail.load('/dev/stdin')
mailbox = TMail::UNIXMbox.new(ARGV[0], nil, true)
mailbox.each_port do |port|
  mail = TMail::Mail.new(port)
  puts "From:    #{mail.from}"
  puts "To:      #{mail.to}"
  puts "Subject: #{mail.subject}"
  
  puts "content type: #{mail.content_type}"
  puts "disposition: #{mail.disposition}"
  puts "body size: #{mail.body.size}"
  puts "body : #{mail.body}"
  #puts "quoted_body : #{mail.quoted_body}"
  
  # this just gives us each line of the body
  #mail.each do |thing|
  #  puts "each thing=#{thing}"
  #end
  
  if mail.multipart?
    puts "Is Multipart #{mail.parts.size}"
    mail.parts.each do |part|
      puts "  part #{part.class}"
      puts "  #{part.content_type}"
      puts "  attachment? : #{mail.attachment?(part)}"
      puts "  main_type: #{part.main_type}"
      if part.content_type == "image/jpeg"
        puts "going to process: #{part.body.size}"
        #puts part.body
        process_image mail.from, part.body
      end
    end
  end
  
  puts ""
end
