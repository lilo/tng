#!/usr/bin/env python3
"""Tango"""
import logging
from pathlib import Path

import typer

from typing import Optional, List

from rich import print, print_json # pylint: disable=redefined-builtin
from sqlalchemy import create_engine, select
from sqlalchemy.orm import sessionmaker, declarative_base, Session
from sqlalchemy import Boolean, Column, ForeignKey, Integer, String, Float, BLOB, Text, DateTime # pylint: disable=unused-import

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy.orm import relationship

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

app = typer.Typer()

SQLALCHEMY_DATABASE_URL = "sqlite:///tango.sqlite3"

engine = create_engine(SQLALCHEMY_DATABASE_URL)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()


class Resource(Base): # pylint: disable=too-few-public-methods
    """Model for tracking resources."""
    __tablename__ = "resource"
    id: Mapped[int] = mapped_column(primary_key=True)
    comment: Mapped[str] = mapped_column(Text(), nullable=True)
    filename: Mapped[str] = mapped_column(Text())
    start_line: Mapped[Optional[int]]
    end_line: Mapped[Optional[int]]


class Dependency(Base): # pylint: disable=too-few-public-methods
    '''
    Model for tracking dependencides
    '''
    __tablename__ = "dependency"
    id: Mapped[int] = mapped_column(primary_key=True)
    src = mapped_column(ForeignKey("resource.id"))
    dst = mapped_column(ForeignKey("resource.id"))
    title = Column(Text)


class Project:
    def __init__(self, path):
        self.path = path

    def add_resource(self, filename: Path, start_line: int, end_line: int, comment: str = None):
        with Session(engine) as session:
            r = Resource(
                filename=filename,
                start_line=start_line,
                end_line=end_line,
                comment=comment
            )
            session.add_all([r])
            session.commit()
    
    @staticmethod
    def get_for_path(path):
        project_root = ''
        return Project(project_root)


@app.command()
def init_db():
    '''Init Database.
    '''
    Base.metadata.create_all(engine)

@app.command()
def track(src: str, dst: str): # pylint: disable=unused-argument
    '''Track dependency.
    '''
    print(f"{src=} {dst=}")

@app.command()
def ls(filename: Path): # pylint: disable=unused-argument,invalid-name
    """List dependencies.
    """
    st = select(Resource).where(Resource.filename == filename)
    print(f"{filename=}")

@app.command()
def s(filename: Path, ids: List[int]):
    # print(f"{filename=}")
    # print(f"{ids=}")
    d = {"filename": str(filename), "ids": ids}
    print_json(data=d)


@app.command()
def file_status(filename: Path):
    status = [
        {
            "line": 5,
            "hash": "5",
            "sources": 1,
            "dests": 1,
            "status": 0
        },
        {
            "line": 6,
            "hash": "6",
            "sources": 0,
            "dests": 2,
            "status": 1
        },
        {
            "line": 30,
            "hash": "30",
            "sources": 1,
            "dests": 1,
            "status": 0
        },
        {
            "line": 32,
            "hash": "32",
            "sources": 1,
            "dests": 1,
            "status": 0
        },
        {
            "line": 110,
            "hash": "110",
            "sources": 1,
            "dests": 1,
            "status": 0
        }
    ]
    print_json(data=status)


@app.command()
def add(filename: Path, start: int = 0, end: int = 0, comment: str=""):
    p = Project(path=".")

@app.command()
def status():
    print("status")

@app.command()
def add_tadd(filename: Path, linenos: List[int]):
    p = Project(path=".")
    p.add_resource(
        filename=filename,
        start_line=min(linenos),
        end_line=max(linenos)
    )
    print("Resource added.")

if __name__ == "__main__":
    app()
