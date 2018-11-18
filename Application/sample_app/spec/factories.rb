FactoryBot.define do
  factory :user do
    name {"Fake User"}
    email {"Fake@user.com"}
    password {"foobar12"}
    password_confirmation {"foobar12"}
  end
end