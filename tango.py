#!/usr/bin/env python3
"""Tango"""
import logging

import typer

from rich import print # pylint: disable=redefined-builtin
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy import Boolean, Column, ForeignKey, Integer, String, Float, BLOB, Text, DateTime # pylint: disable=unused-import

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

app = typer.Typer()

SQLALCHEMY_DATABASE_URL = "sqlite:///tango.sqlite3"

engine = create_engine(SQLALCHEMY_DATABASE_URL)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()


class Dependency(Base): # pylint: disable=too-few-public-methods
    '''
    Model for tracking dependencides
    '''
    __tablename__ = "dependency"
    id = Column(Integer, primary_key=True, index=True)
    src_filename = Column(Text)
    src_line_start = Column(Integer)
    src_line_end = Column(Integer)
    src_pos_start = Column(Integer)
    src_pos_end = Column(Integer)
    src_line_end = Column(Integer)
    title = Column(Text)


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
def ls(filename: str): # pylint: disable=unused-argument,invalid-name
    """List dependencies.
    """
    print(f"{filename=}")

if __name__ == "__main__":
    app()
