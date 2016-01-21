namespace Microsoft.VisualStudio.Project
{
	public abstract partial class ProjectNode
	{
		/// <summary>
		/// Create dependent file node based on an msbuild item
		/// </summary>
		/// <param name="item">msbuild item</param>
		/// <returns>dependent file node</returns>
		public virtual FileNode CreateDependentFileNode(ProjectElement item)
		{
			// ������� ������������ ��� � DependentFileNode �� FileNode
			// ��� ��������� ������������ NemerleDependentFileNode -> NemerleFileNode
	
			return new DependentFileNode(this, item);
		}

		/// <summary>
		/// Create a dependent file node based on a string.
		/// </summary>
		/// <param name="file">filename of the new dependent file node</param>
		/// <returns>Dependent node added</returns>
		public virtual FileNode CreateDependentFileNode(string file)
		{
			// ������� ������������ ��� � DependentFileNode �� FileNode
			// ��� ��������� ������������ NemerleDependentFileNode -> NemerleFileNode

			ProjectElement item = this.AddFileToMsBuild(file);
			return this.CreateDependentFileNode(item);
		}
	}
}
