using System;
using Microsoft.VisualStudio.Designer.Interfaces;
using Nemerle.Compiler;
using Microsoft.VisualStudio.Project;
using System.Diagnostics;

namespace Nemerle.VisualStudio.Project
{
	internal class NemerleVSMDProvider : IVSMDCodeDomProvider
	{
		private readonly FileNode _nemerleFileNode;

		public NemerleVSMDProvider(FileNode nemerleFileNode)
		{
			Trace.Assert(nemerleFileNode is NemerleFileNode || nemerleFileNode is NemerleDependentFileNode);
			_nemerleFileNode = nemerleFileNode;
		}

		#region IVSMDCodeDomProvider Members

		object IVSMDCodeDomProvider.CodeDomProvider
		{
			get
			{
				// ���������� ��������. ValdD2: �� ����� ���� ������ ��� ����� �������� ������, 
				// �� ���� �� �� ����� ��� ������ ���������.
				var nemerleDependentFileNode = _nemerleFileNode as NemerleDependentFileNode;

				if (nemerleDependentFileNode != null)
					return nemerleDependentFileNode.CodeDomProvider;

				var nemerleFileNode = _nemerleFileNode as NemerleFileNode;

				if (nemerleFileNode != null)
					return nemerleFileNode.CodeDomProvider;

				return null;
			}
		}

		#endregion
	}
}
