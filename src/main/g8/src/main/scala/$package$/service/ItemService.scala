package $package$.service

import zio._
import $package$.domain._

trait ItemService:
  def addItem(description: String): IO[DomainError, ItemId]

  def deleteItem(id: String): IO[DomainError, Unit]

  def getAllItems(): IO[DomainError, List[Item]]

  def getItemById(id: String): IO[DomainError, Option[Item]]

  def getItemsByIds(ids: Set[String]): IO[DomainError, List[Item]]

  def updateItem(id: String, description: String): IO[DomainError, Unit]

object ItemService:
  def addItem(description: String): ZIO[Has[ItemService], DomainError, ItemId] =
    ZIO.serviceWith[ItemService](_.addItem(description))

  def deleteItem(id: String): ZIO[Has[ItemService], DomainError, Unit] =
    ZIO.serviceWith[ItemService](_.deleteItem(id))

  def getAllItems(): ZIO[Has[ItemService], DomainError, List[Item]] =
    ZIO.serviceWith[ItemService](_.getAllItems())

  def getItemById(id: String): ZIO[Has[ItemService], DomainError, Option[Item]] =
    ZIO.serviceWith[ItemService](_.getItemById(id))

  def getItemsByIds(ids: Set[String]): ZIO[Has[ItemService], DomainError, List[Item]] =
    ZIO.serviceWith[ItemService](_.getItemsByIds(ids))

  def updateItem(
      id: String,
      description: String,
    ): ZIO[Has[ItemService], DomainError, Unit] =
    ZIO.serviceWith[ItemService](_.updateItem(id, description))