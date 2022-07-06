from time import time
from music21 import *
import pandas as pd
from fractions import Fraction
import os


def mixed_fraction(fractstr):
    temp = fractstr.split(" ")
    if len(temp) == 1:
        return float(Fraction(fractstr))
    return float(temp[0])+float(Fraction(temp[1]))

def add_note_labels(notes_df):
    notes_df["rank"] = pd.to_numeric(notes_df["onset"].rank(method = "dense"), downcast  ="integer")
    note_labels = []
    for i in range(len(notes_df)):
        rank = notes_df["rank"].values[i]
        nl = notes_df["note_label"].values[i]
        #print("{}{}".format(notes_df["rank"][i], notes_df["note_labels"][i]))
        label = "{}{}".format(rank, nl)
        note_labels.append(label)
    notes_df = notes_df.drop("rank", axis = 1)
    notes_df["note_label"] = note_labels
    return notes_df
   
def xml_to_mcsv_by_class(piece, voice, first_measure, last_measure, event_type = "Note", transpose = 0):
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
    last_time_signature = None
    note_labels = []
    for n in part.recurse().getElementsByClass("Rest"):
        if n.activeSite.timeSignature is not None:
            last_time_signature = mixed_fraction(n.activeSite.timeSignature.ratioString)
            break
    if last_time_signature is None:
        for n in part.recurse().getElementsByClass("Note"):
            if n.activeSite.timeSignature is not None:
                last_time_signature = mixed_fraction(n.activeSite.timeSignature.ratioString)
                break
    #if last_time_signature is None:
        #last_time_signature = mixed_fraction("2/1")
    for n in part.recurse().getElementsByClass(event_type):
        if n.tie is None or n.tie.type == "start":
            count += 1
            labels = [""]
            if n.activeSite.timeSignature is not None:
                last_time_signature = mixed_fraction(n.activeSite.timeSignature.ratioString)
            if event_type == "Chord":
                pitches = [_.ps for _ in n.pitches]
                labels = [chr(i + 97) for i in range(len(n.pitches))]
            else:
                pitches = [n.pitch.ps]
            for idx, p in enumerate(pitches):
                duration.append(float(n.duration.quarterLength)/4)
                print(type(n.duration.quarterLength))
                print(float(n.duration.quarterLength)/4)
                beat_pos.append(mixed_fraction(n.beatStr))
                measure_number.append(n.measureNumber)
                time_signature.append(last_time_signature*4)
                onsets.append(n.activeSite.offset + n.offset)
                midi_pitch.append(int(p)-transpose)
                note_number.append(count)
                note_labels.append(labels[idx])
                
    return pd.DataFrame({"note_label": note_labels, "note_number": note_number, "measure_number": measure_number, "beat_pos": beat_pos, "midi_pitch": midi_pitch, "duration": duration,  "time_signature": time_signature, "onset": onsets})

def xml_to_mcsv(piece, voice, first_measure, last_measure, path = "C:/Users/dassc/Desktop/MPIAE/note_data_csv", transpose = 0):
    notes = xml_to_mcsv_by_class(piece, voice, first_measure, last_measure, "Note", transpose = transpose)
    chords = xml_to_mcsv_by_class(piece, voice, first_measure, last_measure, "Chord", transpose = transpose)
    notes_csv = pd.concat([notes, chords])
    notes_csv = notes_csv.sort_values(by = ["onset", "midi_pitch"])
    notes_csv = add_note_labels(notes_csv).drop("note_number", axis = 1)
    return notes_csv.to_csv(os.path.join(path, "a3_hs5-6.csv"), index = False, header = True)

def export_all_scores(path = "C:/Users/dassc/Desktop/MPIAE/note_data_csv"):
    agnus = converter.parse("C:/Users/dassc/Desktop/MPIAE/xml/dufay_missa_agnus.xml")
    kyrie = converter.parse("C:/Users/dassc/Desktop/MPIAE/xml/dufay_missa_kyrie.xml")
    dung = converter.parse("C:/Users/dassc/Desktop/MPIAE/xml/josquin_dung_aultre.xml")
    virgo = converter.parse("C:/Users/dassc/Desktop/MPIAE/xml/josquin_virgo_prudentissima.xml")
    gloria = converter.parse("C:/Users/dassc/Desktop/MPIAE/xml/dufay_missa_gloria.xml")
    xml_to_mcsv(agnus, 2, 97, 150, path)
export_all_scores()


