package expts.controller;

import expts.model.Entry;
import java.util.Collections;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("/entry")
@CrossOrigin
public class EntryController {
  @GetMapping
  public Page<Entry> getEntries() {
    return new PageImpl<>(Collections.emptyList());
  }
}
