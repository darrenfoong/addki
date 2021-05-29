package addki.controller;

import addki.model.Entry;
import addki.request.CollectEntriesRequest;
import addki.service.EntryService;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@CrossOrigin
public class EntryController {
  @Autowired private EntryService entryService;

  @GetMapping("/entry")
  public Page<Entry> getEntries(int number, int size) {
    return entryService.getEntries(number, size);
  }

  @GetMapping("/language")
  public List<String> getLanguages() {
    return entryService.getLanguages();
  }

  @GetMapping(path = "/entry/print/{language}")
  public ResponseEntity<?> printEntries(@PathVariable String language) {
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
  public ResponseEntity<Void> collectEntry(
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
