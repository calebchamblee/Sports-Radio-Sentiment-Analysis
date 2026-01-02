import assemblyai as aai
from pydub import AudioSegment
import os

os.chdir(r"C:/Users/chamb/Downloads")

aai.settings.api_key = token
# audio_file = "./loca_file.mp3"
files = ["t1l.mp3", "t2l.mp3", "t3l.mp3", "t4l.mp3"]

# Stitch episode together
combined = AudioSegment.from_mp3(files[0])
for f in files[1:]:
    combined += AudioSegment.from_mp3(f)

combined.export("stitched.mp3", format="mp3")
audio_file = "stitched.mp3"

config = aai.TranscriptionConfig(speaker_labels=True)
transcriber = aai.Transcriber()
transcript = transcriber.transcribe(audio_file, config)

# Make sure it succeeded before using it
if transcript.status == "error":
    raise RuntimeError(f"Transcription failed: {transcript.error}")

# Save to text file
with open("T&H-10-6-25.txt", "w", encoding="utf-8") as f:
    for utterance in transcript.utterances:
        f.write(f"Speaker {utterance.speaker}: {utterance.text}\n")

print("Diarized transcript saved to date.txt")
