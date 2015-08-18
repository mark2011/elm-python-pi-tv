# elm-python-pi-tv
home photo show, info center and automation - exploits elm language, python, raspberry pi, hdmi/cec tv, web apis

- photo show
  - next photo every 15 seconds
  - display small clock overlay for first 10 seconds of every minute

- remote control buttons
  - left - go backward one photo
  - right - go forward one photo
  - rewind - go to the first photo

- clock full display every 5 minutes

A python script (run.py) launches chromium with the html file (elm-python-pi-tv.html) that is compiled from the elm file (elm-python-pi.tv.elm).
The python script then runs a falcon web service that provides linux services to the elm program.

# Road Map

- gather and display info using web apis - todoist, quip, insteon, philips hue
