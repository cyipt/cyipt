# aim: batch resize images to multiple sizes
# dependency:
library(magick)
# define image sizes:
sizes = c(large = 800, medium = 460, small = 240, thumbnail = 100)

# list files, create name
dir = "../cyipt-website/images/infrastructure/"
f = list.files(path = dir, full.names = TRUE)
image_name = gsub(pattern = dir, replacement =  "", x = f[i])
filetype = ".jpg"
image_name = gsub("/", "", image_name)
image_name = gsub(".png|.jpg", "", image_name)
image_name = paste0(image_name, filetype)

# initialise counters
i = 1
j = 1

# create sub-folder, extract name
new_dir = file.path(dir, names(sizes)[j])
dir.create(new_dir)
new_path = file.path(new_dir, image_name)

# resize image
original_image = magick::image_read(f[i])
w = geometry_size_pixels(width = 800)
new_image = image_resize(raw_image, geometry = w)
image_write(image = new_image, path = new_path)

batch_resize = function(dir, sizes = c(large = 800, medium = 460, small = 240, thumbnail = 100), filetype = ".jpg") {
  f = list.files(path = dir, full.names = TRUE, pattern = "*.png|*.jpg", ignore.case = TRUE)
  image_name = gsub(pattern = dir, replacement =  "", x = f)
  image_name = gsub("/", "", image_name)
  image_name = gsub(".png|.jpg", "", image_name, ignore.case = TRUE)
  image_name = paste0(image_name, filetype)
  for(j in seq_along(sizes)) {
    new_dir = file.path(dir, names(sizes)[j])
    dir.create(new_dir)
    new_path = file.path(new_dir, image_name)
    for(i in seq_along(f)) {
      original_image = magick::image_read(f[i])
      w = geometry_size_pixels(width = sizes[j])
      new_image = image_resize(raw_image, geometry = w)
      image_write(image = new_image, path = new_path[i])
    }
  }

}

batch_resize(dir = "../cyipt-website/images/infrastructure/")
