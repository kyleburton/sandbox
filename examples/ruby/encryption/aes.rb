require 'rubygems'
require 'openssl'
require 'base64'

def decrypt(encrypted_data, key, iv, cipher_type)
  aes = OpenSSL::Cipher::Cipher.new(cipher_type)
  aes.decrypt
  aes.key = Digest::SHA256.digest(key)
  aes.iv = iv if iv != nil
  aes.update(Base64.decode64(encrypted_data)) + aes.final  
end

def encrypt(data, key, iv, cipher_type)
  aes = OpenSSL::Cipher::Cipher.new(cipher_type)
  aes.encrypt
  aes.key = Digest::SHA256.digest(key)
  aes.iv = iv if iv != nil
  Base64.encode64(aes.update(data) + aes.final)
end


# require 'ruby-debug'; debugger;
algo = "AES-256-CBC"
key = "c3b22740-5668-012f-0ec7-58b035effe61"

val = encrypt('foof','blarf',nil,algo)
puts val
puts decrypt(val,'blarf',nil,algo)
