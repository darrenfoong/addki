package addki.controller;

import addki.request.CollectEntriesFormRequest;
import addki.service.EntryService;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
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
    model.addAttribute("languages", entryService.getLanguages());
    model.addAttribute("collectEntriesFormRequest", new CollectEntriesFormRequest());
    return "index";
  }

  @PostMapping
  public String collectEntries(CollectEntriesFormRequest collectEntriesFormRequest) {
    // \r\n is the standard: https://www.w3.org/TR/html401/interact/forms.html#h-17.13.4
    List<String> words = List.of(collectEntriesFormRequest.getWords().split("\r\n"));

    entryService.collectEntries(words, collectEntriesFormRequest.getLanguage());

    return "redirect:/";
  }

  @GetMapping("/logout")
	public String logout(HttpServletRequest httpServletRequest) {
	  HttpSession httpServletRequestSession = httpServletRequest.getSession();
	  httpServletRequestSession.invalidate();

	  return "redirect:/";
  }
}
