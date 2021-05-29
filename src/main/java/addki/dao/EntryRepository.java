package addki.dao;

import addki.model.Entry;
import addki.model.EntryStatus;
import addki.model.projection.LanguageOnly;
import java.util.List;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface EntryRepository extends PagingAndSortingRepository<Entry, Long> {
  List<Entry> findByLanguageAndStatus(String language, EntryStatus status);

  List<LanguageOnly> findDistinctByStatus(EntryStatus status);

  void deleteByStatus(EntryStatus status);
}
