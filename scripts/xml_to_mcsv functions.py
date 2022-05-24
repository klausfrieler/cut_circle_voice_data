from music21 import *
import pandas as pd
from fractions import Fraction

def mixed_fraction(fractstr):
    temp = fractstr.split(" ")
    if len(temp) == 1:
        return float(Fraction(fractstr))
    return float(temp[0])+float(Fraction(temp[1]))

def xml_to_mcsv(piece, voice, first_measure, last_measure):
    count = 1
    voice = piece.parts[voice]
    part = voice.measures(first_measure, last_measure)
    onsets = []
    measure_number = []
    beat_pos = []
    midi_pitch = []
    duration = []
    note_number = []
    time_signature = []
    ts = [mixed_fraction(_.ratioString) for _ in part.recurse().getElementsByClass(meter.TimeSignature)]
    for n in part.recurse().getElementsByClass("Note"):
        if n.tie is None or n.tie.type == "start":
            note_number.append(count)
            count += 1
            duration.append(n.duration.quarterLength/4)
            midi_pitch.append(n.pitch.ps)
            beat_pos.append(mixed_fraction(n.beatStr))
            measure_number.append(n.measureNumber)
            time_signature.append(ts[0])
            onsets.append((n.measureNumber-1)*ts[0]+mixed_fraction(n.beatStr)-1)
    return pd.DataFrame({"note_number": note_number, "measure_number": measure_number, "beat_pos": beat_pos, "midi_pitch": midi_pitch, "duration": duration,  "time_signature": time_signature, "onsets": onsets})
xml_to_mcsv(piece, voice, first_measure, last_measure).to_csv("name.csv", index=False, header=True)
