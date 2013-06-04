BigPixel
========

BigPixel is an image editor for pixel art. It is aimed at creating graphics assets for retro or Minecraft-style 2D games. It supports the creation of pixelated images with 8x8 big pixels in 256 colours with 16 levels of transparency. It is an OpenGL-based cross-platform application. BigPixel currently only supports the BMP image format.

As BigPixel is a plain OpenGL application without platform-specific GUI support, you need to start it as a command line program and supply it with the name of a BMP image file as its sole command line argument. If the file exists, it will be opened for editing; otherwise, a new file will be created. All changes made to an image are persistent — i.e., reflected in the on disk image without explicit saving.

    Left mouse button          — draw with current colour
    Left mouse button + Shift  — erase with transparency
    Right mouse button         — erase with transparency
    'W', 'S', 'A', 'D'         — enlarge canvas to the top, bottom, left, and right, respectively
    'W', 'S', 'A', 'D' + Shift — shrink canvas from the top, bottom, left, and right, respectively

**WARNING:** There is currently no undo facility! Make copies of image files if you are unsure whether you like to keep the changes. (However, if you shrink the visible canvas, the removed content can be restored by simply enlarging the canvas again.)

![Screenshot of BigPixel](images/BigPixel-Island.png)
