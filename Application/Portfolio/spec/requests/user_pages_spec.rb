require 'rails_helper'
require './spec/support/utilities'

RSpec.configure do |c|
  c.include Utilities
end


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

  describe "signup" do

    before { visit signup_path }

    let(:submit) { "Create my account" }

    describe "with invalid information" do
      it "should not create a user" do
        expect { click_button submit }.not_to change(User, :count)
      end
    end

    describe "with valid information" do
      before do
        fill_in "Name", with: "Example User"
        fill_in "Email", with: "user@example.com"
        fill_in "Password",  with: "foobar12"
        fill_in "Confirmation", with: "foobar12"
      end

      it "should create a user" do
        expect { click_button submit }.to change(User, :count).by(1)
      end

      describe "after saving the user" do
        before { click_button submit }
        let(:user) { User.find_by(email: "user@example.com") }

        it { should have_link("Sign out") }
        it { should have_title(user.name) }
        it { should have_selector("div.alert.alert-success", text: "Welcome") }
      end
    end
  end

  describe "edit" do
    let(:user) {FactoryBot.create(:user)}
    before do
      sign_in user
      visit edit_user_path(user)
    end

    describe "page" do
      it {should have_content("Update your profile")}
    end

    describe "with valid information" do
      let(:new_name) {"new name"}
      let(:new_email) {"new@email.com"}
      before do
        fill_in "Name", with: new_name
        fill_in "Email", with: new_email
        fill_in "Password", with: user.password
        fill_in "Confirmation", with: user.password
        click_button "Save changes"
      end

      it {should have_selector('div.alert.alert-success')}
      it {should have_link('Sign out', href: signout_path)}
      it {expect(user.reload.name).to eq new_name}
      it {expect(user.reload.email).to eq new_email}
    end

    describe "with invalid information" do
      before {click_button "Save changes"}
      it {should have_content('error')}
      it {should have_selector('div.alert.alert-danger')}
    end
  end

  describe "index" do
    let(:user) { FactoryBot.create(:user) }
    before(:each) do
      sign_in user
      visit users_path
    end

    it { should have_content('All users') }

    describe "pagination" do

      before(:all) { 30.times { FactoryBot.create(:user) } }
      after(:all)  { User.delete_all }

      it { should have_selector('div.pagination') }

      it "should list each user" do
        User.paginate(page: 1).each do |user|
          expect(page).to have_selector('li', text: user.name)
        end
      end
    end

    describe "as an admin" do
      let (:admin) {FactoryBot.create(:admin)}
      before do
        sign_in admin
        visit users_path
      end

      it {should have_link('delete', href: user_path(User.first))}
      it "should be able to delete another user" do
        expect do
          click_link('delete', match: :first)
        end.to change(User, :count).by(-1)
      end
      it {should_not have_link('delete', href: user_path(admin))}
    end
  end
end
