require 'rails_helper'
require 'capybara/rails'


describe "Authentication" do

  subject { page }

  describe "sign in page" do
    before { visit signin_path }

    describe "with invalid information" do
      before{click_button "Sign in"}
      it {should have_content('Sign in') }
      it {should have_selector('div.alert.alert-danger')}
      describe "after visiting another page" do
        before {click_link "Home"}
        it {should_not have_selector('div.alert.alert-danger')}
      end
    end
    describe "with valid information" do
      let(:user) {FactoryBot.create(:user)}
      before do
        #fake a sign in
        fill_in "Email", with: user.email.upcase
        fill_in "Password", with: user.password
        click_button "Sign in"
      end

      it {should have_link('Profile', href: user_path(user))}
      it {should have_link('Sign out', href: signout_path)}

      describe "followed by signout" do
        before {click_link "Sign out"}
        it {should have_link("Sign in")}
      end
    end
  end
end
