import pandas as pd
import pytest

import prune_peps as pp


@pytest.fixture
def db(tmp_path):
    path = str(tmp_path / "test.sqlite")
    pp.save_table_to_db(pd.DataFrame({"peptide": ["AAAA", "AXXA", "XXXX"], "score": [1, 2, 3]}), path, "4")
    pp.save_table_to_db(pd.DataFrame({"peptide": ["AAAAA", "AAAAX"], "score": [4, 5]}), path, "5")
    return path


def test_get_table_names(db):
    assert sorted(pp.get_table_names(db)) == ["4", "5"]


def test_save_and_get_table_roundtrip(db):
    df = pp.get_table_from_db(db, "4")
    assert list(df.columns) == ["peptide", "score"]
    assert df.peptide.tolist() == ["AAAA", "AXXA", "XXXX"]


def test_save_table_replaces_existing(db):
    pp.save_table_to_db(pd.DataFrame({"peptide": ["GG"]}), db, "4")
    assert pp.get_table_from_db(db, "4").peptide.tolist() == ["GG"]


def test_get_table_from_db_missing_table_returns_none(db):
    assert pp.get_table_from_db(db, "does_not_exist") is None


def test_get_columns_from_db(db):
    df = pp.get_columns_from_db(db, "4", ["peptide"])
    assert list(df.columns) == ["peptide"]
    assert len(df) == 3


def test_get_columns_from_db_missing_column_returns_none(db):
    assert pp.get_columns_from_db(db, "4", ["nope"]) is None


def test_delete_rows_based_on_column_items(db):
    pp.delete_rows_based_on_column_items(db, "4", "peptide", ["AXXA", "XXXX"])
    assert pp.get_table_from_db(db, "4").peptide.tolist() == ["AAAA"]


def test_delete_rows_with_empty_items_is_noop(db):
    pp.delete_rows_based_on_column_items(db, "4", "peptide", [])
    assert len(pp.get_table_from_db(db, "4")) == 3


def test_vacuum_db_keeps_data(db):
    pp.vacuum_db(db)
    assert len(pp.get_table_from_db(db, "5")) == 2


def test_prune_peps_removes_peptides_above_threshold(db):
    pp.prune_peps(db, prcnt=0.2)
    # table "4": X-fraction 0, 0.5, 1.0 -> only AAAA survives
    assert pp.get_table_from_db(db, "4").peptide.tolist() == ["AAAA"]
    # table "5": AAAAX has fraction exactly 0.2, which is not > 0.2
    assert pp.get_table_from_db(db, "5").peptide.tolist() == ["AAAAA", "AAAAX"]


def test_prune_peps_stricter_threshold(db):
    pp.prune_peps(db, prcnt=0.1)
    assert pp.get_table_from_db(db, "5").peptide.tolist() == ["AAAAA"]


def test_prune_peps_uses_peptide_length_not_table_name(tmp_path):
    path = str(tmp_path / "t.sqlite")
    pp.save_table_to_db(pd.DataFrame({"peptide": ["AAAA", "XXAA"]}), path, "peptides")
    pp.prune_peps(path, prcnt=0.2)
    assert pp.get_table_from_db(path, "peptides").peptide.tolist() == ["AAAA"]
