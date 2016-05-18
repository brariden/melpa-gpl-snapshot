﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Controls;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Nemerle.Compiler;
using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.LanguageService.Highlighting.TypeClassifier
{
  public sealed class TypeClassifier : IClassifier
  {
    private readonly ITextBuffer _textBuffer;
    private static readonly HashSet<string> _predefinedTypes = new HashSet<string>(StringComparer.InvariantCulture)
      { "object", "int", "string", "void", "bool", "list", "byte", "float", "uint", "char", 
        "ulong", "ushort", "decimal", "sbyte", "short", "double", "long" };

    public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;

    public TypeClassifier(
      IStandardClassificationService standardClassification,
      IClassificationTypeRegistryService classificationRegistry,
      ITextBuffer textBuffer)
    {
      _textBuffer          = textBuffer;
      _userStyle           = classificationRegistry.GetClassificationType(ClassificationTypes.UserTypeName);
      _predefinedStyle     = standardClassification.Keyword;
      _textBuffer.Changed += TextBuffer_Changed;
    }


    private void TextBuffer_Changed(object sender, TextContentChangedEventArgs e)
    {
    }

    public void RedrawTypeHighlighting()
    {
      var currentSnapshot = _textBuffer.CurrentSnapshot;
      OnClassificationChanged(new SnapshotSpan(currentSnapshot, 0, currentSnapshot.Length));
    }

    private void OnClassificationChanged(SnapshotSpan span)
    {
      var handler = ClassificationChanged;
      if (null != handler)
        handler(this, new ClassificationChangedEventArgs(span));
    }

    public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
    {
      NemerleSource source;
      if (!_textBuffer.Properties.TryGetProperty(typeof(NemerleSource), out source))
        return ClassifierUtils.EmptyClassifications;

      if (source.TypeLocations == null)
      {
        source.TypeLocations = new List<Location>();
        var engine = source.GetEngine();
        engine.BeginUpdateTypeHighlightings(source);
        return ClassifierUtils.EmptyClassifications;
      }

      var fileIndex            = source.FileIndex;
      var snapshot             = span.Snapshot;
      var loc                  = Utils.ToNLocation(fileIndex, span);
      var locationsToHighlight = GetLocationsToHighlight(source, loc).ToArray();
      if (locationsToHighlight.Length == 0)
        return ClassifierUtils.EmptyClassifications;

      var result = new List<ClassificationSpan>();
      foreach (var location in locationsToHighlight)
      {
        if (!location.IsSourceAvailable)
          continue;
        
        var highlightSpan  = Utils.NLocationToSpan(snapshot, location);
        var text           = snapshot.GetText(highlightSpan);
        var classification = _predefinedTypes.Contains(text) ? _predefinedStyle : _userStyle;
        result.Add(new ClassificationSpan(new SnapshotSpan(snapshot, highlightSpan), classification));
      }
      return result;
    }

    private static IEnumerable<Location> GetLocationsToHighlight(NemerleSource source, Location loc)
    {
      foreach (var x in source.MethodsTypeLocations)
      {
        if (x.Item1.IsIntersect(loc))
          foreach (var loc2 in x.Item2)
            yield return loc2;
      }

      foreach (var x in source.TypeLocations)
        if (x.IsIntersect(loc))
          yield return x;
    }


    private static ClassificationSpan MakeClassificationSpan(ITextSnapshot textSnapshot, Span span, IClassificationType classificationType)
    {
      return new ClassificationSpan(new SnapshotSpan(textSnapshot, span), classificationType);
    }

    private IClassificationType _predefinedStyle;
    private IClassificationType _userStyle;
  }
}
