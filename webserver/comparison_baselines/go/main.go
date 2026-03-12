package main

import (
	"log"
	"net/http"
	"os"
	"path/filepath"
)

func mustRead(path string) []byte {
	b, err := os.ReadFile(path)
	if err != nil {
		log.Fatalf("read %s: %v", path, err)
	}
	return b
}

func main() {
	root, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	bench := filepath.Join(filepath.Dir(root), "bench_files")
	json1k := mustRead(filepath.Join(bench, "json1k.json"))
	file50k := mustRead(filepath.Join(bench, "file50k.bin"))
	file1m := mustRead(filepath.Join(bench, "file1m.bin"))

	mux := http.NewServeMux()

	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		_, _ = w.Write([]byte("ok\n"))
	})

	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		_, _ = w.Write([]byte("healthy\n"))
	})

	mux.HandleFunc("/json", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write(json1k)
	})

	mux.HandleFunc("/file50k", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		_, _ = w.Write(file50k)
	})

	mux.HandleFunc("/file1m", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		_, _ = w.Write(file1m)
	})

	log.Println("Go baseline on :8084")
	log.Fatal(http.ListenAndServe("127.0.0.1:8084", mux))
}
