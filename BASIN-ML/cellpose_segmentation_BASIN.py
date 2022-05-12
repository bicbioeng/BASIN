import numpy as np
import time, os, sys
from urllib.parse import urlparse
import skimage.io
from inspect import getsourcefile

from urllib.parse import urlparse
from cellpose import models, plot

# text file containing image paths
in_file_dir = os.path.join(os.path.dirname(getsourcefile(lambda:0)),"basinCellposeImgs.txt")
input_file = open(in_file_dir,"r")
files = input_file.read().splitlines()
imgs = [skimage.io.imread(f) for f in files]
input_file.close()
nimg = len(imgs)

imgs_2D = imgs
# convert grayscale to rgb
for i in range(0,len(imgs)):
  if imgs[i].ndim < 3:
    imgs[i] = plot.image_to_rgb(imgs[i])

#model = models.Cellpose(gpu = False, model_type='cyto')
model = models.CellposeModel(gpu = False, pretrained_model = os.path.join(os.path.dirname(
  getsourcefile(lambda:0)),"www","cellpose_residual_on_style_on_concatenation_off_Manually_curated_Images_and_Masks_2021_01_08_21_36_17.184931"))

channels_r = [1,0]
channels_g = [2,0]
channels_b = [3,0]

# get masks for red, green, and blue channels
masks_r, flows, styles = model.eval(imgs_2D, diameter=None, flow_threshold=None, channels = channels_r)
masks_g, flows, styles = model.eval(imgs_2D, diameter=None, flow_threshold=None, channels = channels_g)
masks_b, flows, styles = model.eval(imgs_2D, diameter=None, flow_threshold=None, channels = channels_b)

# computes the mask for a single image, channels 1-3 are RGB and 0 is grayscale
def compute_mask(img, channel):
  c = None
  if channel == 0:
    c = [0,0]
  elif channel == 1:
    c = [1,0]
  elif channel == 2:
    c = [2,0]
  else:
    c = [3,0]
  masks, flows, styles = model.eval(img,diameter=None, flow_threshold=None, channels = c)
  return masks
