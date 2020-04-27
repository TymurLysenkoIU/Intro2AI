# Path to the image to be processed
IMAGE_BASE_PATH = './img/'
IMAGE_NAME = 'ricardo_1.jpg'
IMAGE_PATH = IMAGE_BASE_PATH + IMAGE_NAME

# Visual parameters
# Number of vertices for polygons, which are composed into an image; half-open interval
MIN_NUM_POLYGON_VERTICES = 3   # should be >= 3
MAX_NUM_POLYGON_VERTICES = 17

# Sizes of the circumscribed circle around a polygon; half-open interval
MIN_CIRCUMSCRIBED_CIRCLE_SIZE = 5   # px
MAX_CIRCUMSCRIBED_CIRCLE_SIZE = 31  # px

# Initial number of polygons on the image
INITIAL_NUM_POLYGONS_SCALE = 0.05

# Number of images in the population
POPULATION_SIZE = 20
