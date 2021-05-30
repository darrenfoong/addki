package addki.controller;

import addki.service.EntryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class EntryController {
  @Autowired private EntryService entryService;

  @GetMapping
  public String getEntries(
      @RequestParam(defaultValue = "0") int number,
      @RequestParam(defaultValue = "10") int size,
      Model model) {
    model.addAttribute("entries", entryService.getEntries(number, size));
    return "index";
  }
}
