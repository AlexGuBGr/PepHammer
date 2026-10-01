import sqlite3

import pandas as pd
import pytest

import prep_pepti_data as ppd

VOCAB = ppd.vocab


def write_fasta(dirpath, name, records):
    text = "".join(">{}\n{}\n".format(h, s) for h, s in records)
    (dirpath / name).write_text(text, encoding="utf-8")


@pytest.fixture
def fasta_dir(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)  # get_peptides writes a pickle in cwd
    d = tmp_path / "fasta"
    d.mkdir()
    write_fasta(d, "Antibacterial.fasta", [("id1", "ACDE"), ("id2", "acde"), ("id3", "AXXXXXXX")])
    write_fasta(d, "Hormone.fasta", [("id4", "ACDE"), ("id5", "GGGG"), ("id6", "A"), ("id7", "AC1E")])
    return str(d) + "/", ["Antibacterial.fasta", "Hormone.fasta"]


def test_get_peptides_merges_functions_and_uppercases(fasta_dir):
    dirr, files = fasta_dir
    out = ppd.get_peptides(dirr, files, VOCAB)
    assert {f for f, _ in out["ACDE"]} == {"Antibacterial", "Hormone"}


def test_get_peptides_filters_invalid(fasta_dir):
    dirr, files = fasta_dir
    out = ppd.get_peptides(dirr, files, VOCAB)
    assert set(out) == {"ACDE", "GGGG"}  # too many X, too short and illegal characters are dropped


def test_get_peptides_x_threshold(fasta_dir):
    dirr, files = fasta_dir
    out = ppd.get_peptides(dirr, files, VOCAB, prcnt=1.0)
    assert "AXXXXXXX" in out


def test_get_peptides_writes_pickle(fasta_dir, tmp_path):
    dirr, files = fasta_dir
    ppd.get_peptides(dirr, files, VOCAB)
    assert (tmp_path / "bioactivty_dct.pickle").exists()


def test_make_table_strings(tmp_path):
    dct = {
        "GGGG": {("Hormone", "id5")},
        "ACDE": {("Antibacterial", "id1"), ("Hormone", "id4")},
        "AC": {("Hormone", "id9")},
    }
    db = str(tmp_path / "out.sqlite")
    ppd.make_table_strings(dct, ["Antibacterial", "Hormone"], db, "peptipedia")
    con = sqlite3.connect(db)
    df = pd.read_sql("SELECT * FROM peptipedia", con)
    con.close()

    # sorted by length then sequence
    assert df.peptide.tolist() == ["AC", "ACDE", "GGGG"]
    assert df.Length.tolist() == [2, 4, 4]
    acde = df[df.peptide == "ACDE"].bioactivities.iloc[0]
    assert set(acde.split(";")) == {"Antibacterial", "Hormone"}


def test_make_table_writes_indicator_columns(tmp_path):
    dct = {"ACDE": {("Antibacterial", "id1"), ("Hormone", "id4")}, "AC": {("Hormone", "id9")}}
    db = str(tmp_path / "out.sqlite")
    ppd.make_table(dct, ["Antibacterial", "Hormone"], db, "peptipedia")
    con = sqlite3.connect(db)
    df = pd.read_sql("SELECT * FROM peptipedia", con)
    con.close()
    assert df.peptide.tolist() == ["AC", "ACDE"]
    assert df.Hormone.tolist() == [1, 1]
    assert df.Antibacterial.tolist() == [0, 1]
