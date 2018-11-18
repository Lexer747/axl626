require 'rails_helper'
require 'capybara/rails'

describe "User pages" do

  subject { page }

  describe "profile page" do
    let(:user) {FactoryBot.create(:user)}
    before {visit user_path(user)}

    it {should have_content(user.name)}
  end

  describe "signup page" do
    before { visit signup_path }

    it { should have_content('Sign up') }
  end
end
