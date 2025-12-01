// ZynML Example: Audio Processing Pipeline
// Demonstrates audio loading and feature extraction for ML
// Run with: zynml run examples/audio_pipeline.ml

fn main() {
    println("=== ZynML Audio Pipeline ===")

    // Load audio file
    let audio = audio_load("test.wav")

    // Get audio info
    let sr = audio_sample_rate(audio)
    let duration = audio_duration(audio)

    println("Sample rate: ")
    println("Duration: ")

    // Resample to 16kHz (standard for speech)
    let resampled = audio |> resample(16000)

    // Convert to mono
    let mono = resampled |> to_mono()

    // Normalize amplitude
    let normalized = mono |> normalize()

    // Extract mel spectrogram features
    // Parameters: n_mels=80, n_fft=400, hop_length=160
    let mel = normalized |> mel_spectrogram(80, 400, 160)

    println("Mel spectrogram extracted")
    println("Ready for ML model input")

    // Cleanup
    audio_free(audio)
}
