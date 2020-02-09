package ru.tinkoff.phobos.configured

final case class ElementCodecConfig(transformAttributeNames: String => String,
                                    transformElementNames: String => String) with
  def withElementsRenamed(transform: String => String): ElementCodecConfig   = copy(transformElementNames = transform)
  def withAttributesRenamed(transform: String => String): ElementCodecConfig = copy(transformAttributeNames = transform)
  def withStyle(transform: String => String): ElementCodecConfig = copy(transformElementNames = transform, transformAttributeNames = transform)


object ElementCodecConfig with
  val default: ElementCodecConfig = ElementCodecConfig(identity, identity)
