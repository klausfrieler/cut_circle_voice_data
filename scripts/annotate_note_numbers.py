def annotate_note_numbers(piece, voice, first_measure, last_measure):
    count = 1
    voice = piece.parts[voice]
    part = voice.measures(first_measure, last_measure)
    for n in part.recurse().getElementsByClass("Note"):
        if n.tie is None or n.tie.type == "start":
            n.addLyric(str(count))
            count += 1
    count = 1

