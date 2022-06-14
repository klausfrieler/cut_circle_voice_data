def mixed_fraction(fractstr):
    temp = fractstr.split(" ")
    if len(temp) == 1:
        return float(Fraction(fractstr))
    return float(temp[0])+float(Fraction(temp[1]))

def add_note_labels(notes_df):
    notes_df["rank"] = pd.to_numeric(notes_df["onsets"].rank(method = "dense"), downcast  ="integer")
    note_labels = []
    for i in range(len(notes_df)):
        print("{}{}".format(notes_df["rank"][i], notes_df["note_labels"][i]))
        note_labels.append("{}{}".format(notes_df["rank"][i], notes_df["note_labels"][i]))
    notes_df = notes_df.drop("rank", axis = 1)
    notes_df["note_labels"] = note_labels
    return notes_df
   
def xml_to_mcsv_by_class(piece, voice, first_measure, last_measure, event_type = "Note"):
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
    for n in part.recurse().getElementsByClass(event_type):
        if n.tie is None or n.tie.type == "start":
            count += 1
            labels = [""]
            if n.activeSite.timeSignature is not None:
                last_time_signature = mixed_fraction(n.activeSite.timeSignature.ratioString)
            if event_type == "Chord":
                pitches = [_.ps for _ in n.pitches]
                labels = [chr(i + 97) for i in range(0, n.pitches)]
            else:
                pitches = [n.pitch.ps]
            for idx, p in enumerate(pitches):
                duration.append(n.duration.quarterLength/4)
                beat_pos.append(mixed_fraction(n.beatStr))
                measure_number.append(n.measureNumber)
                time_signature.append(last_time_signature*4)
                onsets.append(n.activeSite.offset + n.offset)
                midi_pitch.append(p)
                note_number.append(count)
                note_labels.append(labels[idx])
                
    return pd.DataFrame({"note_number": note_number, "measure_number": measure_number, "beat_pos": beat_pos, "midi_pitch": midi_pitch, "duration": duration,  "time_signature": time_signature, "onsets": onsets, "note_labels": note_labels})

    def xml_to_mcsv(piece, voice, first_measure, last_measure):
        notes = xml_to_mcsv_by_class(piece, voice, first_measure, last_measure, "Note")
        chords = xml_to_mcsv_by_class(piece, voice, first_measure, last_measure, "Chord")
        notes_csv = pd.concat([notes, chords])
        notes_csv = notes_csv.sort_values(by = ["onsets", "midi_pitch"])
        notes_csv = add_note_labels(notes_csv)
        return notes_csv
