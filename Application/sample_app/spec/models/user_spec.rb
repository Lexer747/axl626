require 'rails_helper'
require 'spec_helper'
require 'capybara/rails'

describe User do
  before do
    @user = User.new(name: "Test User", email: "user@test.com", password: "qwerty12345", password_confirmation: "qwerty12345")
  end

  subject {@user}

  #respond_to checks if a given attribute is a part of the object, i.e. has getters and setters
  it {should respond_to(:name)}
  it {should respond_to(:email)}
  it {should respond_to(:password_digest)} #from has_secure_password
  it {should respond_to(:password)} #from has_secure_password
  it {should respond_to(:password_confirmation)} #from has_secure_password
  it {should respond_to(:authenticate)} #from has_secure_password
  it {should respond_to(:remeber_token)}

  it {should be_valid} #calls .valid? on the object

  describe "when null name passed" do
    before {@user.name = " "}
    it {should_not be_valid}
  end
  describe "when name is too long" do
    before {@user.name = "a" * 51}
    it {should_not be_valid}
  end
  describe "when null email passed" do
    before {@user.email = " "}
    it {should_not be_valid}
  end
  describe "when email format is invalid" do
    it "should be invalid" do
      addresses = %w[foo@bar.+com foo+bar@.com foo@a_bar.com foo@bar,com foo.bar@com. foo@.com]
      addresses.each do |i|
        @user.email = i
        expect(@user).not_to be_valid
      end
    end
  end
  describe "when email format is valid" do
    it "should be valid" do
      addresses = %w[foo@bar.com foobar@nil.com foo.bar@ab.com f@aaa.bb.com]
      addresses.each do |i|
        @user.email = i
        expect(@user).to be_valid
      end
    end
  end
  describe "when email address is already taken" do
    before do
      duplicate1 = @user.dup
      duplicate2 = @user.dup
      duplicate1.save
      duplicate2.email = duplicate2.email.upcase
      duplicate2.save
    end
    it {should_not be_valid}
  end
  describe "when email address is saved it should be lower case" do
    let(:test_email) {"FoO@ExAmPle.CoM"}
    it "should be all lower case" do
      @user.email = test_email
      @user.save
      expect(@user.reload.email).to eq test_email.downcase
    end
  end
  describe "when null password passed" do
    before {@user = User.new(name: "Foo", email: "bar@foo.com", password: "", password_confirmation: "")}
    it {should_not be_valid}
  end
  describe "when the password doesn't match" do
    before {@user.password_confirmation = @user.password + "fail"}
    it {should_not be_valid}
  end

  describe "when authenticate is executed" do
    before {@user.save}
    let(:found_user) {User.find_by(email: @user.email) }
    describe "with a valid password" do
      it {should eq found_user.authenticate(@user.password)}
    end
    describe "with an invalid password" do
      let(:invalid_auth) {found_user.authenticate(@user.password + "fail")}

      it {should_not eq invalid_auth} #user != invalid_auth
      specify {expect(invalid_auth).to eq false}
    end
  end
end