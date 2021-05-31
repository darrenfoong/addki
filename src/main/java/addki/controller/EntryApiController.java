package addki.controller;

import addki.model.Entry;
import addki.request.CollectEntriesRequest;
import addki.service.EntryService;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
public class EntryApiController {
  @Autowired private EntryService entryService;

  @GetMapping("/entry")
  public Page<Entry> getEntries(@RequestParam int number, @RequestParam int size) {
    return entryService.getEntries(number, size);
  }

  @GetMapping("/language")
  public List<String> getLanguages() {
    return entryService.getSupportedLanguages();
  }

  @GetMapping(path = "/entry/print")
  public ResponseEntity<?> printEntries(@RequestParam String language) {
    return ResponseEntity.ok()
        .contentType(MediaType.APPLICATION_OCTET_STREAM)
        .header(
            HttpHeaders.CONTENT_DISPOSITION,
            "attachment; filename=\""
                + language
                + "_"
                + Instant.now().toString().replaceAll("[:.]", "")
                + ".txt"
                + "\"")
        .body(entryService.printEntries(language).getBytes(StandardCharsets.UTF_8));
  }

  @PostMapping("/entry/collect")
  public ResponseEntity<Void> collectEntries(
      @RequestBody CollectEntriesRequest collectEntriesRequest) {
    entryService.collectEntries(
        collectEntriesRequest.getWords(), collectEntriesRequest.getLanguage());

    return ResponseEntity.ok().build();
  }

  @PostMapping("/entry/cleanup")
  public ResponseEntity<Void> cleanup() {
    entryService.cleanup();

    return ResponseEntity.ok().build();
  }
}
