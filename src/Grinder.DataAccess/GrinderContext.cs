using System.IO;
using Microsoft.EntityFrameworkCore;

namespace Grinder.DataAccess
{
    public class GrinderContext : DbContext
    {
        public DbSet<User> Users { get; set; }

        public static void MigrateUp()
        {
            Directory.CreateDirectory(Path.Combine(Directory.GetCurrentDirectory(), "data"));
            using (var context = new GrinderContext())
            {
                context.Database.Migrate();
            }
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<User>()
                .HasIndex(b => b.Username)
                .IsUnique();
            modelBuilder.Entity<User>()
                .HasIndex(b => b.UserId)
                .IsUnique();
        }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            optionsBuilder.UseSqlite("Data Source=./data/grinder.db");
        }
    }
}