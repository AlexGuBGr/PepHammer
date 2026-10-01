import pandas as pd
import pytest

import make_figtables as mf


@pytest.fixture
def strings_db(tmp_path):
    path = str(tmp_path / "strings.sqlite")
    df = pd.DataFrame({
        "peptide": ["AA", "CC", "DD", "EEE"],
        "Length": [2, 2, 2, 3],
        "bioactivities": ["a;b", "a", "b", "a;b"],
    })
    mf.save_table_to_db(df, path, "peptipedia")
    return path


@pytest.fixture
def pred_db(tmp_path):
    path = str(tmp_path / "pred.sqlite")
    df = pd.DataFrame({
        "peptide": ["AA", "CC", "DDD"],
        "Length": [2, 2, 3],
        "a": [0.9, 0.2, 0.6],
        "b": [0.1, 0.8, 0.7],
    })
    mf.save_table_to_db(df, path, "preds")
    return path


def test_save_and_get_table(tmp_path):
    path = str(tmp_path / "t.sqlite")
    mf.save_table_to_db(pd.DataFrame({"x": [1, 2]}), path, "t")
    assert mf.get_table_from_db(path, "t").x.tolist() == [1, 2]


def test_get_table_missing_returns_none(tmp_path):
    path = str(tmp_path / "t.sqlite")
    mf.save_table_to_db(pd.DataFrame({"x": [1]}), path, "t")
    assert mf.get_table_from_db(path, "nope") is None


def test_length_vs_bioact_strings(strings_db):
    mf.make_legnth_vs_bioact(strings_db, "peptipedia", ["a", "b"], strr=True)
    out = mf.get_table_from_db(strings_db, "length_bio_dist").set_index("Pep_length")
    assert out.loc[2, "a"] == 2 and out.loc[2, "b"] == 2
    assert out.loc[3, "a"] == 1 and out.loc[3, "b"] == 1


def test_length_vs_bioact_thresholds(pred_db):
    mf.make_legnth_vs_bioact(pred_db, "preds", ["a", "b"], thrs=[0.5, 0.8])
    out = mf.get_table_from_db(pred_db, "length_bio_dist")
    assert set(out.pred) == {">0.5", ">0.8"}
    lo = out[out.pred == ">0.5"].set_index("Pep_length")
    assert lo.loc[2, "a"] == 1 and lo.loc[2, "b"] == 1
    assert lo.loc[3, "a"] == 1 and lo.loc[3, "b"] == 1
    hi = out[out.pred == ">0.8"].set_index("Pep_length")
    assert hi.loc[2, "a"] == 1 and hi.loc[2, "b"] == 0


def test_length_vs_bioact_excludes_length_one_and_over_150(tmp_path):
    path = str(tmp_path / "p.sqlite")
    df = pd.DataFrame({"peptide": ["A", "AA", "G" * 151], "Length": [1, 2, 151], "a": [1.0, 1.0, 1.0]})
    mf.save_table_to_db(df, path, "preds")
    mf.make_legnth_vs_bioact(path, "preds", ["a"], thrs=[0.5])
    out = mf.get_table_from_db(path, "length_bio_dist")
    assert out.Pep_length.tolist() == [2]


def test_bio_across_peptipedia(strings_db):
    mf.make_bio_across_peptipedia(strings_db, "peptipedia", ["a", "b"])
    out = mf.get_table_from_db(strings_db, "bioact_vs_bioact").set_index("rownames")
    # a and b each occur in 3 peptides, together in 2
    assert out.loc["a", "a"] == 3
    assert out.loc["b", "b"] == 3
    assert out.loc["a", "b"] == out.loc["b", "a"] == 2


def test_bio_across_matrix(pred_db):
    mf.make_bio_across_matrix(pred_db, "preds", ["a", "b"], thrs=[0.5])
    out = mf.get_table_from_db(pred_db, "bioact_vs_bioact")
    assert set(out.pred) == {">0.5"}
    assert set(out.rownames) == {"a", "b"}
    a_row = out[out.rownames == "a"].iloc[0]
    # peptides with a > 0.5: AA (b=0.1) and DDD (b=0.7)
    assert a_row["a"] == 2 and a_row["b"] == 1
