FactoryBot.define do
  factory :user do
    sequence(:name) {|n| "User \##{n}"}
    sequence(:email) {|n| "User_\##{n}@fake.com"}
    password {"12345678"}
    password_confirmation {"12345678"}

    factory :admin do
      admin {true}
    end
  end
end