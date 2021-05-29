package expts.dao;

import expts.model.Entry;
import expts.model.EntryStatus;
import java.util.List;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface EntryRepository extends PagingAndSortingRepository<Entry, Long> {
  List<Entry> findByLanguageAndStatus(String language, EntryStatus status);

  void deleteByStatus(EntryStatus status);
}
