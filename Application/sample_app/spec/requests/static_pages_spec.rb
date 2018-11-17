require 'rails_helper'
require 'capybara/rails'

RSpec.describe "StaticPages" do
  subject {page}
  describe "Home page" do
    before {visit root_path}
    it {should have_content('Sample App')}
    it {should have_http_status(200)}
  end
  describe "Help page" do
    before {visit help_path}
    it {should have_content('Help')}
    it {should have_http_status(200)}
  end
  describe "About page" do
    before {visit about_path}
    it {should have_content('About Us')}
    it {should have_http_status(200)}
  end
  describe "Contact page" do
    before {visit contact_path}
    it {should have_content('Contact')}
    it {should have_http_status(200)}
  end
end
