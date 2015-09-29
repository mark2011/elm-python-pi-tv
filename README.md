# elm-python-pi-tv
home photo show, info center and automation - exploits elm language, python, raspberry pi, hdmi/cec tv, web apis

- initial menu of channels
  - remote control
    - up/down - select channel
    - ok - go to selected channel
    - exit - (in channel) - return to channel menu
- photo show channel
  - next photo every 15 seconds
  - display small clock overlay for first 10 seconds of every minute
  - remote control buttons
    - left - go backward one photo
    - right - go forward one photo
    - rewind - go to the first photo
  - full clock display every 5 minutes

- Philips Hue control channel
  - api token is in configuration file /home/pi/.config/elm-python-pi-tv.json
    - file format: {"hueToken" : "TOKEN"}
  - list of lights
    - red - light is not powered
    - blue - light is not on
    - white light is illuminated
  - remote control buttons
    - up/down to move from light to light
    - ok - blink selected light

A python script (run.py) launches chromium with the html file (elm-python-pi-tv.html) that is compiled from the elm file (elm-python-pi.tv.elm).
The python script then runs a falcon web service that provides linux services to the elm program.

- pictures are stored in /home/pi/Pictures
- configuration is in /home/pi/.config/elm-python-pi-tv.json

# Road Map

- gather and display info using web apis - todoist, quip, insteon
