PhotoClassifier
================

Yet another Haskell pet project.
PhotoClassifier is a tool to automatically create a hierarchy of photos (symlinks) based on the date of the photo.

What this means, is that from a Photo's EXIF info, you could have a hierarchy like this:
(e.g. Photo.jpg taken 2010-01-07)

output-dir/
    | Processed
        | Photo.jpg
    | 2010
        | 01
            | Photo.jpg -> ../../Photo.jpg
        | Week01 
            | Photo.jpg -> ../../Photo.jpg

The week number is taken from the startDate parameter (-s) to PhotoClassifier


Status
------

Still not very usable.

DEPS
----

- missingh
- exif

COMPILE
-------

ghc -o PhotoClassifier --make Main.hs

RUN
---

PhotoClassifier -i <pic_directory>
