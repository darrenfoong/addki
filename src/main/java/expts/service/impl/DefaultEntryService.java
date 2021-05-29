package expts.service.impl;

import expts.dao.EntryRepository;
import expts.service.EntryService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class DefaultEntryService implements EntryService {
  @Autowired private EntryRepository entryRepository;
}
