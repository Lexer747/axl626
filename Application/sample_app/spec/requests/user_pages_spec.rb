require 'rails_helper'
require 'capybara/rails'

describe "User pages" do

  subject { page }

  describe "signup page" do
    before { visit signup_path }

    it { should have_content('Sign up') }
  end
end
