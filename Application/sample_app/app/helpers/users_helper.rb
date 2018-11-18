module UsersHelper
  def avatar_for(user)
    image = "rails.png"
    image_tag(image, alt: user.name, class: "avatar")
  end
end
