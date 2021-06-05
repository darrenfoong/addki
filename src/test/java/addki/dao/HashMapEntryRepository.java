package addki.dao;

import addki.model.Entry;
import addki.model.EntryStatus;
import addki.model.projection.LanguageOnly;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.Getter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

public class HashMapEntryRepository implements EntryRepository {
  @Getter private Map<Long, Entry> map = new HashMap<>();

  @Override
  public List<Entry> findByLanguageAndStatus(String language, EntryStatus status) {
    return null;
  }

  @Override
  public List<LanguageOnly> findDistinctByStatus(EntryStatus status) {
    return null;
  }

  @Override
  public void deleteByStatus(EntryStatus status) {}

  @Override
  public Iterable<Entry> findAll(Sort sort) {
    return null;
  }

  @Override
  public Page<Entry> findAll(Pageable pageable) {
    return null;
  }

  @Override
  public <S extends Entry> S save(S entity) {
    Long id = (long) map.size();

    entity.setId(id);
    map.put(id, entity);

    return entity;
  }

  @Override
  public <S extends Entry> Iterable<S> saveAll(Iterable<S> entities) {
    return null;
  }

  @Override
  public Optional<Entry> findById(Long aLong) {
    return Optional.empty();
  }

  @Override
  public boolean existsById(Long aLong) {
    return false;
  }

  @Override
  public Iterable<Entry> findAll() {
    return null;
  }

  @Override
  public Iterable<Entry> findAllById(Iterable<Long> longs) {
    return null;
  }

  @Override
  public long count() {
    return 0;
  }

  @Override
  public void deleteById(Long aLong) {}

  @Override
  public void delete(Entry entity) {}

  @Override
  public void deleteAll(Iterable<? extends Entry> entities) {}

  @Override
  public void deleteAll() {}
}
