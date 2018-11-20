namespace :db do
  desc "Fill database with sample data"
  task populate: :environment do
    User.create!(name: "Example User",
                 email: "example@fake.org",
                 password: "12345678",
                 password_confirmation: "12345678")
    99.times do |n|
      name  = Faker::Name.name
      email = "example-#{n+1}@fake.org"
      password  = "password"
      User.create!(name: name,
                   email: email,
                   password: password,
                   password_confirmation: password)
    end
  end
end