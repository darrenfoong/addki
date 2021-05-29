package addki.config;

import addki.service.EntryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Scheduled;

@Slf4j
@Configuration
public class CleanupTask {
  @Autowired private EntryService entryService;

  @Scheduled(fixedDelayString = "${cleanup.delay}")
  public void cleanup() {
    entryService.cleanup();
  }
}
