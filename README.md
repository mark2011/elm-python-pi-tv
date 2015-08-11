# elm-python-pi-tv
home photo show, info center and automation - exploits elm language, python, raspberry pi, hdmi/cec tv, web apis

A python script (run.py) launches chromium with the html file (elm-python-pi-tv.html) that is compiled from the elm file (elm-python-pi.tv.elm).
The python script then runs a falcon web service that the elm program polls for a list of photos,
and the elm program displays each one for 15 seconds. Every 5 minutes a clock screen is displayed.

# Road Map

- python reads cec-client for remote control signals and sends them as keystrokes to chromium
using the xdotool program
- gather and display info using web apis - todoist, quip, insteon, philips hue
