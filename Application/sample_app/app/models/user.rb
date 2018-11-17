class User < ApplicationRecord
  validates :name, presence: true, length: {maximum: 50}
  EMAIL_RFC_5322 = /\A([\w+\-].?)+@[a-z\d\-]+(\.[a-z]+)*\.[a-z]+\z/i
  #The official regex for emails according to the RFC 5322
  validates :email, presence: true, format: {with: EMAIL_RFC_5322}, uniqueness: {case_sensitive: false}

  has_secure_password #auto validates the password field, and salts and hashes the password
  validates :password, length: {minimum: 8}, allow_nil: false #add 2 mroe validations to password

  before_save {self.email = email.downcase} #save all emails in our db as lowercase
end
